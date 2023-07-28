unit Unit1;

{
example taken from here https://delphi.cz/post/VirtualDrawTree.aspx
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, laz.VirtualTrees;

type
  TItemType = (itRectangle, itOval, itTriangle);

  PTreeItem=^TTreeItem;
  TTreeItem = Record
    item: TItemType;
  end;

  { TForm1 }

  TForm1 = class(TForm)
    vt: TLazVirtualDrawTree;
    procedure FormCreate(Sender: TObject);
    procedure vtBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure vtDrawNode(Sender: TBaseVirtualTree; const PaintInfo: TVTPaintInfo
      );
    procedure vtExpanded(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vtFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
  procedure mAddItem(DrawItem: TItemType);
  var
    pNode: PVirtualNode;
    pData: PTreeItem;
  begin
    // родительский узел
    vt.ChildCount[vt.RootNode] := vt.ChildCount[vt.RootNode] + 1;
    vt.ValidateNode(vt.RootNode, False);

    pNode := vt.RootNode^.LastChild;
    pData :=  PTreeItem(vt.GetNodeData(pNode));
    pData^.item := DrawItem;
    vt.NodeHeight[pNode] := 20;

    // дочерний узел
    vt.ChildCount[pNode] := 1;
    vt.ValidateNode(pNode, True);
    pData :=  PTreeItem(vt.GetNodeData(pNode^.FirstChild));
    pData^.item := DrawItem;
    vt.NodeHeight[pNode^.FirstChild] := 30; // calculated later
  end;
begin
  vt.NodeDataSize := SizeOf(TTreeItem);
  mAddItem(itRectangle);
  mAddItem(itOval);
  mAddItem(itTriangle);
end;

procedure TForm1.vtBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
var
  Data:PTreeItem;
  r: TRect;
  iHeight: Integer;
begin
  Data:= Sender.GetNodeData(Node);
  if (Data = nil) then Exit;
  if vt.GetNodeLevel(Node) <> 1 then // мы установим только дочерний узел
    Exit;
  case Data^.item of
    itRectangle:
      iHeight := 90;
    itOval:
      iHeight := 20;
    itTriangle:
      iHeight := 50;
  end;

  if vt.NodeHeight[Node] <> iHeight then
    vt.NodeHeight[Node] := iHeight;
end;

procedure TForm1.vtDrawNode(Sender: TBaseVirtualTree; const PaintInfo: TVTPaintInfo);
var
  Data:PTreeItem;
  r: TRect;
begin
  Data:= Sender.GetNodeData(PaintInfo.Node);
  if (Data = nil) then
    Exit;
  r := PaintInfo.ContentRect;
  inc(r.Top, 3);
  case vt.GetNodeLevel(PaintInfo.Node) of
  0:
    begin
      PaintInfo.Canvas.Font.Color := clRed;
      case Data^.item of
        itRectangle:
          PaintInfo.Canvas.TextOut(r.Left, r.Top, 'Прямоугольник');
        itOval:
          PaintInfo.Canvas.TextOut(r.Left, r.Top, 'Овал');
        itTriangle:
          PaintInfo.Canvas.TextOut(r.Left, r.Top, 'Треугольник');
      end;
    end;
  1:begin
      case Data^.item of
        itRectangle:
          begin
            PaintInfo.Canvas.Brush.Color := clGreen;
            PaintInfo.Canvas.Rectangle(r.Left, r.Top, 100, r.Top + 80);
          end;
        itOval:
          begin
            PaintInfo.Canvas.Brush.Color := clYellow;
            PaintInfo.Canvas.Ellipse(r.Left + 20, r.Top, r.Left + 50, r.Bottom);
          end;
        itTriangle:
          begin
            PaintInfo.Canvas.Brush.Color := clGradientActiveCaption;
            PaintInfo.Canvas.Polygon([Point(r.Left, r.Top), Point(r.Left + 40, r.Top),
              Point(r.Left + 20, r.Bottom)]);
          end;
      end;
    end;
  end;
end;

procedure TForm1.vtExpanded(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  vt.Refresh;
end;

procedure TForm1.vtFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data:PTreeItem;
begin
  Data:= Sender.GetNodeData(Node);
  if (Data = nil) then
    Exit;
  Finalize(Data^);
end;

end.

