unit GizmosLblCbo;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls;

type

  { TGizmosLblCbo }

  TGizmosLblCbo = class(TCustomPanel)
  private
    FLbl: TLabel;
    FCbo: TComboBox;
    property Caption;
  protected

  public
    constructor Create(TheOwner: TComponent); override;
    procedure Paint; override;
  published
    property Align;
    property Anchors;
    property Font;
    property Lbl: TLabel read FLbl write FLbl;
    property Cbo: TComboBox read FCbo write FCbo;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Gizmos',[TGizmosLblCbo]);
end;

{ TGizmosLblCbo }

constructor TGizmosLblCbo.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  Self.BevelOuter := bvNone;
  Self.ChildSizing.VerticalSpacing := 4;
  Self.Caption := '';

  FCbo := TComboBox.Create(Self);
  FCbo.Parent := Self;
  FCbo.Align := alBottom;

  FLbl := TLabel.Create(Self);
  FLbl.Parent := Self;
  FLbl.Align := alClient;
  FLbl.Layout := tlCenter;
  FLbl.FocusControl := FCbo;
end;

procedure TGizmosLblCbo.Paint;
begin
  inherited Paint;
end;

end.
