{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit gizmos;

{$warn 5023 off : no warning about unused units}
interface

uses
  GizmoCalendar, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('GizmoCalendar', @GizmoCalendar.Register);
end;

initialization
  RegisterPackage('gizmos', @Register);
end.
