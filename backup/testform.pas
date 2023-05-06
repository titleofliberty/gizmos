unit testform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, EditBtn, Arrow,
  StdCtrls, ExtCtrls, GizmoCalendar, SimpleCard, Types, Contnrs, DateUtils,
  htmlcolors;

type

  { TfrmTest }

  TfrmTest = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Panel1: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    idx : Integer;
  public

  end;

var
  frmTest: TfrmTest;

implementation

{$R *.lfm}

{ TfrmTest }

procedure TfrmTest.FormCreate(Sender: TObject);
begin
  idx := 0;
end;

procedure TfrmTest.Button1Click(Sender: TObject);
begin
  if idx > 0 then Dec(idx);
  Panel1.Color := htmlcolorsarray[idx].Color;
  Panel1.Caption := Trim(htmlcolorsarray[idx].Name);
end;

procedure TfrmTest.Button2Click(Sender: TObject);
begin
  if idx < 141 then Inc(idx);
  Panel1.Color := htmlcolorsarray[idx].Color;
  Panel1.Caption := Trim(htmlcolorsarray[idx].Name);
end;

end.

