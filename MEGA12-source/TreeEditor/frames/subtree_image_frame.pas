{
	Copyright 1992-2024 Sudhir Kumar and Koichiro Tamura

	This file is part of the MEGA (Molecular Evolutionary Genetics Analyis) software.

	MEGA (Molecular Evolutionary Genetics Analysis) is free software:
	you can redistribute it and/or modify it under the terms of the
	GNU General Public License as published by the Free Software
	Foundation, either version 3 of the License, or (at your option)
	any later version.

	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with this program.  If not, see <https://www.gnu.org/licenses/>.

   Contributor(s):   The MEGA source code and software is made available in the hopes that it will be useful. 
   In keeping with the spirit and intent that the MEGA project is developed under, the authors of MEGA request that before
   distributing any significant changes to the MEGA source code (or derivatives thereof), you share
   those changes with the MEGA authors so that they may have the opportunity to test that
   the changes do not introduce errors into the code or otherwise negatively impact the correctness
   or performance of the MEGA software.
   
	Please email inqiries to s.kumar@temple.edu
}

unit subtree_image_frame;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, frame_utils,
  Dialogs, ExtDlgs, MTreeBox, MegaConsts;

type

  { TSubtreeImageFrame }

  TSubtreeImageFrame = class(TFrame, ITreeToolbarFrame, IDisablesEventNotifications)
    ImageLoadBtn: TButton;
    ImageClearBtn: TButton;
    ImageExportBtn: TButton;
    OpenImageDlg: TOpenPictureDialog;
    SaveImgDlg: TSavePictureDialog;
    ShowImageCkBx: TCheckBox;
    ImagePositionCBx: TComboBox;
    Image1: TImage;
    Image: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    procedure ImageClearBtnClick(Sender: TObject);
    procedure ImageClick(Sender: TObject);
    procedure ImageExportBtnClick(Sender: TObject);
    procedure ImageLoadBtnClick(Sender: TObject);
  private
    FDisableEventNotifications: Boolean;
    FOptionsChangedNotify: TNotifyEvent;
    FPanel2Height: Integer;
    procedure SetOptionsChangedNotify(AValue: TNotifyEvent);
  public
    function GetHeaderPanel: TPanel;
    function GetBodyPanel: TPanel;
    function GetExpandCollapseImage: TImage;
    procedure ToggleCollapseBodyPanel;
    procedure SetParentControl(aParent: TWinControl);
    procedure SetAlignVal(aAlign: TAlign);

    procedure SetSubtreeAttrib(NodeAttrib: TNodeAttrib);
    procedure GetSubtreeAttrib(var NodeAttrib: TNodeAttrib);
    function NotificationsDisabled: Boolean;

    property OptionsChangedNotify: TNotifyEvent read FOptionsChangedNotify write SetOptionsChangedNotify;
  end;

implementation

uses
  Graphics;

{$R *.lfm}

{ TSubtreeImageFrame }

procedure TSubtreeImageFrame.ImageLoadBtnClick(Sender: TObject);
var
 JpegImage: TJPEGImage = nil;
begin
  if not OpenImageDlg.Execute then exit;
  try
    try
      if UpperCase(ExtractFileExt(OpenImageDlg.FileName)) = '.BMP' then
        Image.Picture.Bitmap.LoadFromFile(OpenImageDlg.FileName)
      else
      begin
        JpegImage := TJPEGImage.Create;
        JpegImage.LoadFromFile(OpenImageDlg.FileName);
        Image.Picture.Bitmap.Assign(JpegImage);
      end;
    finally
      ImageExportBtn.Enabled := not Image.Picture.Bitmap.Empty;
      ImageClearBtn.Enabled := not Image.Picture.Bitmap.Empty;
      if Assigned(FOptionsChangedNotify) then
        FOptionsChangedNotify(Sender);
    end;
  except
    on E:Exception do
    begin
      ShowMessage('Failed to load image: ' + E.Message);
    end;
  end;
end;

procedure TSubtreeImageFrame.SetOptionsChangedNotify(AValue: TNotifyEvent);
begin
  if FOptionsChangedNotify = AValue then Exit;
  FOptionsChangedNotify := AValue;
  ShowImageCkBx.OnChange := FOptionsChangedNotify;
  ImagePositionCBx.OnChange := FOptionsChangedNotify;
end;

procedure TSubtreeImageFrame.ImageClick(Sender: TObject);
begin

end;

procedure TSubtreeImageFrame.ImageExportBtnClick(Sender: TObject);
var
 JpegImage: TJPEGImage = nil;
begin
  if not SaveImgDlg.Execute then exit;
  try
    try
      if UpperCase(ExtractFileExt(SaveImgDlg.FileName)) = '.BMP' then
        Image.Picture.Bitmap.SaveToFile(SaveImgDlg.FileName)
      else
      begin
        JpegImage := TJPEGImage.Create;
        JpegImage.Assign(Image.Picture.Bitmap);
        JpegImage.SaveToFile(SaveImgDlg.FileName);
      end;
    except
      on E: Exception do
        ShowMessage('Application error when saving image: ' + E.Message);
    end;
  finally
    if Assigned(JpegImage) then
      JpegImage.Free;
  end;
end;

procedure TSubtreeImageFrame.ImageClearBtnClick(Sender: TObject);
var
  NullBitmap: TBitmap = nil;
begin
  try
    try
      NullBitmap := TBitmap.Create;
      Image.Picture.Bitmap.Assign(NullBitmap);
      Image.Refresh;
      if Assigned(FOptionsChangedNotify) then
        FOptionsChangedNotify(Sender);
    except
      on E:Exception do
        ShowMessage('Application error when clearing the image: ' + E.Message);
    end;
  finally
    ImageExportBtn.Enabled := not Image.Picture.Bitmap.Empty;
    ImageClearBtn.Enabled := not Image.Picture.Bitmap.Empty;
    if Assigned(NullBitmap) then
      NullBitmap.Free;
  end;
end;

function TSubtreeImageFrame.GetHeaderPanel: TPanel;
begin
  Result := Panel1;
end;

function TSubtreeImageFrame.GetBodyPanel: TPanel;
begin
  Result := Panel2;
end;

function TSubtreeImageFrame.GetExpandCollapseImage: TImage;
begin
  Result := Image1;
end;

procedure TSubtreeImageFrame.ToggleCollapseBodyPanel;
begin
  if Panel2.Visible then FPanel2Height := Height;
  Panel2.Visible := not Panel2.Visible;
  if Panel2.Visible then
    Height := FPanel2Height
  else
    Height := Panel1.Height;
end;

procedure TSubtreeImageFrame.SetParentControl(aParent: TWinControl);
begin
  Parent := aParent;
end;

procedure TSubtreeImageFrame.SetAlignVal(aAlign: TAlign);
begin
  Align := aAlign;
end;

procedure TSubtreeImageFrame.SetSubtreeAttrib(NodeAttrib: TNodeAttrib);
begin
  try
    FDisableEventNotifications := True;
    ShowImageCkBx.Checked := NodeAttrib.ShowImage;
    case NodeAttrib.GraphicAlign of
      gaRight : ImagePositionCBx.ItemIndex := 0;
      gaLeft  : ImagePositionCBx.ItemIndex := 1;
      gaTop   : ImagePositionCBx.ItemIndex := 2;
      gaBottom: ImagePositionCBx.ItemIndex := 3;
    end;
    Image.Picture.Bitmap.Assign(NodeAttrib.Image);
    ImageExportBtn.Enabled := not Image.Picture.Bitmap.Empty;
    ImageClearBtn.Enabled := not Image.Picture.Bitmap.Empty;
  finally
    FDisableEventNotifications := False;
  end;
end;

procedure TSubtreeImageFrame.GetSubtreeAttrib(var NodeAttrib: TNodeAttrib);
begin
  case ImagePositionCBx.ItemIndex of
    0: NodeAttrib.GraphicAlign := gaRight;
    1: NodeAttrib.GraphicAlign := gaLeft;
    2: NodeAttrib.GraphicAlign := gaTop;
    3: NodeAttrib.GraphicAlign := gaBottom;
  end;
  If Image.Picture.Bitmap.Empty then
    NodeAttrib.ClearImage
  else
    NodeAttrib.Image.Assign(Image.Picture.Bitmap);
  NodeAttrib.ShowImage := ShowImageCkBx.Checked;
end;

function TSubtreeImageFrame.NotificationsDisabled: Boolean;
begin
  Result := FDisableEventNotifications;
end;

end.

