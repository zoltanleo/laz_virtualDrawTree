unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, laz.VirtualTrees;

type

  PMyRec = ^TMyRec;
  TMyRec = record
    k: PtrInt;
    aStr: String;
  end;

  { TForm1 }

  TForm1 = class(TForm)
    imgList: TImageList;
    VDT: TLazVirtualDrawTree;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure VDTAfterPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas);
    procedure VDTBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure VDTDrawNode(Sender: TBaseVirtualTree;
      const PaintInfo: TVTPaintInfo);
    procedure VDTFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VDTGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.VDTGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize:= SizeOf(TMyRec);
end;

procedure TForm1.VDTFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data: PMyRec = nil;
begin
  Data:= Sender.GetNodeData(Node);

  if Assigned(Data) then Finalize(Data^);
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  Node: PVirtualNode = nil;
  Data: PMyRec = nil;
  i: PtrInt = -1;
begin
  for i:= 0 to Pred(imgList.Count) do
  begin
    Node:= VDT.AddChild(nil);
    Data:= VDT.GetNodeData(Node);

    with Data^ do
    begin
      k:= i;
      aStr:= 'строка_' + IntToStr(i);
    end;
  end;

end;

procedure TForm1.FormShow(Sender: TObject);
begin
  //VDT.Repaint;
end;

procedure TForm1.VDTAfterPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas);
begin
  VDT.Header.Columns.Items[0].Width:= VDT.ClientWidth - 10 - imgList.Width;
  VDT.Header.Columns.Items[1].Width:= imgList.Width;

end;

procedure TForm1.VDTBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
begin
  //Sender.NodeHeight[Node]:= imgList.Height;
  Sender.NodeHeight[Node]:= 100;
end;

procedure TForm1.VDTDrawNode(Sender: TBaseVirtualTree; const PaintInfo: TVTPaintInfo);
var
  Data:PMyRec = nil;
  R, Ri: TRect;
  cvs: TCanvas;
  pict: TPicture = nil;
  h: PtrInt;
begin
  Data:= Sender.GetNodeData(PaintInfo.Node);

  if not Assigned(Data) then Exit;

  pict:= TPicture.Create;
  try
    R:= PaintInfo.ContentRect;
    Inc(R.Top,3);
    cvs:= PaintInfo.Canvas;

    case PaintInfo.Column of
      0:
        begin
          h:= cvs.TextHeight(Data^.aStr);
          cvs.Font.Color := clRed;
          cvs.TextOut(R.Left,r.Top + (R.Bottom - h) div 2, Data^.aStr);
        end;
      1:
        begin
          if (PaintInfo.CellRect.Width > PaintInfo.CellRect.Height)
            then h:= PaintInfo.CellRect.Height
            else h:= PaintInfo.CellRect.Width;

          imgList.GetBitmap(Data^.k,pict.Bitmap);
          Ri:= Rect(
            R.Left,
            R.Top,
            R.Left + h
            , R.Top + h
                  );
          cvs.StretchDraw(Ri,pict.Bitmap);
        end;
    end;
  finally
    FreeAndNil(pict);
  end;
end;

end.

