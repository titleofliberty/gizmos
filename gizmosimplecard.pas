unit gizmosimplecard;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls;

type

  { TCardLabel }

  TCardLabel = class(TPersistent)
  private
    FChange   : TNotifyEvent;
    FColor    : TColor;
    FLabel    : TLabel;
    FAlignment: TAlignment;
    FCaption  : TCaption;
    FFont     : TFont;
    FHeight   : Integer;
    FLayout   : TTextLayout;
    procedure SetAlignment(AValue: TAlignment);
    procedure SetCaption(AValue: TCaption);
    procedure SetColor(AValue: TColor);
    procedure SetFont(AValue: TFont);
    procedure SetHeight(AValue: Integer);
    procedure SetLayout(AValue: TTextLayout);
    procedure FontChanged(Sender: TObject);
  protected
    property OnChange: TNotifyEvent read FChange write FChange;
  public
    constructor Create;
  published
    property Caption: TCaption read FCaption write SetCaption;
    property Alignment: TAlignment read FAlignment write SetAlignment;
    property Layout: TTextLayout read FLayout write SetLayout;
    property Font: TFont read FFont write SetFont;
    property Height: Integer read FHeight write SetHeight;
    property Color: TColor read FColor write SetColor;
  end;

  { TSimpleCard }

  TGizmoSimpleCard = class(TPanel)
  private
    FHeader : TCardLabel;
    FFooter : TCardLabel;
    FBody   : TCardLabel;
    FPadding: Integer;
    procedure Paint; override;
    procedure SetBody(AValue: TCardLabel);
    procedure SetFooter(AValue: TCardLabel);
    procedure SetHeader(AValue: TCardLabel);
    procedure SetPadding(AValue: Integer);
    procedure ChildChanged(Sender: TObject);
  protected

  public
    constructor Create(TheOwner: TComponent); override;
  published
    property Header  : TCardLabel read FHeader  write SetHeader;
    property Footer  : TCardLabel read FFooter  write SetFooter;
    property Body    : TCardLabel read FBody    write SetBody;
    property Padding : Integer    read FPadding write SetPadding;
    property Anchors;
    property Align;
  end;

procedure Register;

implementation

procedure Register;
begin
  {$I gizmosimplecard_icon.lrs}
  RegisterComponents('Gizmos',[TGizmoSimpleCard]);
end;

procedure TCardLabel.SetAlignment(AValue: TAlignment);
begin
  if FAlignment = AValue then Exit;
  FAlignment := AValue;
  if Assigned(FChange) then FChange(Self);
end;

procedure TCardLabel.SetCaption(AValue: TCaption);
begin
  if FCaption = AValue then Exit;
  FCaption := AValue;
  if Assigned(FChange) then FChange(Self);
end;

procedure TCardLabel.SetColor(AValue: TColor);
begin
  if FColor = AValue then Exit;
  FColor := AValue;
  if Assigned(FChange) then FChange(Self);
end;

procedure TCardLabel.SetFont(AValue: TFont);
begin
  if FFont = AValue then Exit;
  FFont := AValue;
  if Assigned(FChange) then FChange(Self);
end;

procedure TCardLabel.SetHeight(AValue: Integer);
begin
  if FHeight = AValue then Exit;
  FHeight := AValue;
  if Assigned(FChange) then FChange(Self);
end;

procedure TCardLabel.SetLayout(AValue: TTextLayout);
begin
  if FLayout = AValue then Exit;
  FLayout := AValue;
  if Assigned(FChange) then FChange(Self);
end;

procedure TCardLabel.FontChanged(Sender: TObject);
begin
  if Assigned(FChange) then FChange(Self);
end;

constructor TCardLabel.Create;
begin
  FFont := TFont.Create;
  FFont.OnChange := @FontChanged;

  FColor     := clDefault;
  FCaption   := 'Sample Text';
  FAlignment := taLeftJustify;
  FLayout    := tlCenter;
  FHeight    := 48;
end;

{ TSimpleCard }

constructor TGizmoSimpleCard.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  Caption := '';
  FPadding := 4;
  BevelOuter := bvNone;

  FHeader := TCardLabel.Create;
  FFooter := TCardLabel.Create;
  FBody   := TCardLabel.Create;

  FHeader.OnChange := @ChildChanged;
  FFooter.OnChange := @ChildChanged;
  FBody.OnChange   := @ChildChanged;

  with Header do
  begin
    Caption := 'Header Text';
    Height := 12;
  end;

  with Footer do
  begin
    Caption := 'Footer Text';
    Height := 12;
  end;


  with Body do
  begin
    Caption := 'Body Text';
    Height := Height - (Header.Height + Footer.Height);
  end;

end;

procedure TGizmoSimpleCard.Paint;
var
  hdr, ftr, bdy: TRect;
  ts : TTextStyle;
begin
  inherited Paint;

  ts.Clipping := true;
  ts.SingleLine := true;
  ts.SystemFont := false;
  ts.EndEllipsis := true;

  Canvas.Brush.Color := Self.Color;
  Canvas.FillRect(0, 0, Width, Height);

  hdr := TRect.Create(0, 0, Width, Header.Height);
  ftr := TRect.Create(0, Height - Footer.Height, Width, Height);
  bdy := TRect.Create(0, hdr.Height, Width, Height - ftr.Height);

  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := Header.Color;
  Canvas.FillRect(hdr);
  if (Header.Caption <> '') then
  begin
    hdr.Inflate(FPadding * -1, FPadding * -1);
    Canvas.Brush.Style := bsClear;
    Canvas.Font := Header.Font;
    ts.Alignment := Header.Alignment;
    ts.Layout := Header.Layout;
    Canvas.TextRect(hdr, hdr.Left, hdr.Top, Header.Caption, ts);
  end;

  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := Footer.Color;
  Canvas.FillRect(ftr);
  if (Footer.Caption <> '') then
  begin
    ftr.Inflate(FPadding * -1, FPadding * -1);
    Canvas.Brush.Style := bsClear;
    Canvas.Font := Footer.Font;
    ts.Alignment := Footer.Alignment;
    ts.Layout := Footer.Layout;
    Canvas.TextRect(ftr, ftr.Left, ftr.Top, Footer.Caption, ts);
  end;

  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := Body.Color;
  Canvas.FillRect(bdy);
  if (Body.Caption <> '') then
  begin
    bdy.Inflate(FPadding * -1, FPadding * -1);
    Canvas.Brush.Style := bsClear;
    Canvas.Font := Body.Font;
    ts.Alignment := Body.Alignment;
    ts.Layout := Body.Layout;
    ts.Wordbreak := true;
    ts.SingleLine := false;
    Canvas.TextRect(bdy, bdy.Left, bdy.Top, Body.Caption, ts);
  end;

end;

procedure TGizmoSimpleCard.SetBody(AValue: TCardLabel);
begin
  if FBody = AValue then Exit;
  FBody := AValue;
end;

procedure TGizmoSimpleCard.SetFooter(AValue: TCardLabel);
begin
  if FFooter = AValue then Exit;
  FFooter := AValue;
end;

procedure TGizmoSimpleCard.SetHeader(AValue: TCardLabel);
begin
  if FHeader = AValue then Exit;
  FHeader := AValue;
end;

procedure TGizmoSimpleCard.SetPadding(AValue: Integer);
begin
  if FPadding = AValue then Exit;
  FPadding := AValue;
  Invalidate;
end;

procedure TGizmoSimpleCard.ChildChanged(Sender: TObject);
begin
  Invalidate;
end;

end.
