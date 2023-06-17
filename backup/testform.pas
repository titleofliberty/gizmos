unit testform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  gizmosimplecard, gizmoborderpanel, DateUtils;

type

  { TfrmTest }

  TfrmTest = class(TForm)
    BorderPanel1: TBorderPanel;
  private

  public

  end;

var
  frmTest: TfrmTest;

implementation

{$R *.lfm}

end.

