unit testform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, EditBtn, Arrow,
  StdCtrls, ExtCtrls, GizmoCalendar;

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

end.

