{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit gizmos;

{$warn 5023 off : no warning about unused units}
interface

uses
  GizmoCalendar, gizmosimplecard, htmlcolors, gizmoborderpanel, GizmosLblCbo, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('GizmoCalendar', @GizmoCalendar.Register);
  RegisterUnit('gizmosimplecard', @gizmosimplecard.Register);
  RegisterUnit('gizmoborderpanel', @gizmoborderpanel.Register);
  RegisterUnit('GizmosLblCbo', @GizmosLblCbo.Register);
end;

initialization
  RegisterPackage('gizmos', @Register);
end.
