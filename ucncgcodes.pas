unit uCNCGcodes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uCNCTypes;

function BuildGCodeList: TCNCCodeArray;

implementation

function BuildGCodeList: TCNCCodeArray;
var
  vGCode : TCNCCode;
begin
  Result.Clear;
  // G0 Rapid
  Result.Add(NewCNCCode('G0 ', 'Rapid Movement'));

  // G1 Feed
  Result.Add(NewCNCCode('G1 ', 'Feed Movement'));

  // G4 Dwell
  Result.Add(NewCNCCode('G4 X', 'Dwell for X seconds'));

  // G5 Dialog
  Result.Add(NewCNCCode('G5 T  M ', 'Dialog Message'));

  // G20 inches
  Result.Add(NewCNCCode('G20', 'Inch measurement'));

  // G21 centimeters
  Result.Add(NewCNCCode('G21', 'Centimeter measurement'));

  // G22 millimeters
  Result.Add(NewCNCCode('G22', 'Millimeter measurement'));

  // G28 Home
  Result.Add(NewCNCCode('G28', 'Home'));

  // G40 Compensation off
  Result.Add(NewCNCCode('G40', 'Compensation off'));

  // G41 Compensation left
  Result.Add(NewCNCCode('G41', 'Compensation left'));

  // G42 Compensation right
  Result.Add(NewCNCCode('G42', 'Compensation right'));

  // G43 Tool length offset
  Result.Add(NewCNCCode('G43 Z', 'Tool length offset'));

  // G53 Rapid machine
  Result.Add(NewCNCCode('G53 ', 'Rapid machine movement'));

  // G90 Absolute mode
  Result.Add(NewCNCCode('G90', 'Absolute mode'));

  // G91 Relative mode
  Result.Add(NewCNCCode('G91', 'Relative mode'));

  // G92 Set location
  Result.Add(NewCNCCode('G92 ', 'Set local coordinates'));

  // G92.2 Shift local coordinates
  Result.Add(NewCNCCode('G92.2 ', 'Shift local coordinates'));
end;

end.

