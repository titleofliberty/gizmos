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
  private

  public

  end;

var
  frmTest: TfrmTest;

implementation

{$R *.lfm}

{ TfrmTest }

end.

