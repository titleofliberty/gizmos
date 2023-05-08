unit testform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  GizmoCalendar, DateUtils;

type

  { TfrmTest }

  TfrmTest = class(TForm)
    GizmoCalendar1: TGizmoCalendar;
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  frmTest: TfrmTest;

implementation

{$R *.lfm}

{ TfrmTest }

procedure TfrmTest.FormCreate(Sender: TObject);
var
  d1, d2, d3: TDateTime;
  ds : TDatesList;
begin
  d1 := Now();
  d2 := IncDay(d1, 4);
  d3 := IncDay(d2, 5);

  ds := TDatesList.Create;
  GizmoCalendar1.HighlightDates.Add(d1);
  GizmoCalendar1.HighlightDates.Add(d2);
  GizmoCalendar1.HighlightDates.Add(d3);


end;

end.

