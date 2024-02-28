unit uCNCLMCodes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uCNCTypes;

function BuildGenCNCCodes: TCNCCodeArray;

implementation

function BuildGenCNCCodes: TCNCCodeArray;
begin
  Result.Clear;

  // L20 Enable soft limits
  Result.Add(NewCNCCode('L20', 'Enable soft limits'));

  // L21 Disable soft limits
  Result.Add(NewCNCCode('L21', 'Disable soft limits'));

  // L22 Set low boundaries
  Result.Add(NewCNCCode('L22 ', 'Set low boundaries'));

  // L60 Virtual Input Off
  Result.Add(NewCNCCode('L60 C', 'Turn virtual input off'));

  // L61 Virtual input on
  Result.Add(NewCNCCode('L61 C', 'Turn virtual input on'));

  // L90 One time absolute
  Result.Add(NewCNCCode('L90 ', '* Inline one time absolute'));

  // L91 One time relative
  Result.Add(NewCNCCode('L91 ', '* Inline one time relative'));

  // L210 alternate limit
  Result.Add(NewCNCCode('L210 ', 'Select alternate low limit'));

  // L210.1 alternate limit pin
  Result.Add(NewCNCCode('L210.1 C', 'Select alternate low limit pin'));

  // L211 alternate high limit
  Result.Add(NewCNCCode('L211 ', 'Select alternate high limit'));

  // L211.1 alternate high limit pin
  Result.Add(NewCNCCode('L211.1 C', 'Select alternate high limit pin'));

  // L212 primary limits
  Result.Add(NewCNCCode('L212', 'Select primary limits'));

  // M3 spindle cw
  Result.Add(NewCNCCode('M3', 'Spindle clockwise'));

  // M4 spindle ccw
  Result.Add(NewCNCCode('M4', 'Spindle counter clockwise'));

  // M5 spindle stop
  Result.Add(NewCNCCode('M5', 'Spindle stop'));

  // M6 toolchange
  Result.Add(NewCNCCode('M6 T', 'Tool change'));

  // M7 docking pin up
  Result.Add(NewCNCCode('M7', 'Docking pins up'));

  // M7.1 docking pin down
  Result.Add(NewCNCCode('M7.1', 'Docking pins down'));

  // M8 Extractor on
  Result.Add(NewCNCCode('M8', 'Extractor on'));

  // M8.1 Extractor off
  Result.Add(NewCNCCode('M8.1', 'Extractor off'));

  // M9 Table vacuum on
  Result.Add(NewCNCCode('M9', 'Table vacuum on'));

  // M9.1 Table vacuum off
  Result.Add(NewCNCCode('M9.1', 'Table vacuum off'));

  // M10 Extractor plate down
  Result.Add(NewCNCCode('M10', 'Extractor plate down'));

  // M10.1 Extractor plate up
  Result.Add(NewCNCCode('M10.1', 'Extractor plate up'));

  // M11 output on
  Result.Add(NewCNCCode('M11 C', 'Output on'));

  // M12 output off
  Result.Add(NewCNCCode('M12 C', 'Output off'));

  // M17 Pause until input on
  Result.Add(NewCNCCode('M17 C', 'Pause until input on'));

  // M17.1 Run next if input on
  Result.Add(NewCNCCode('M17.1 C', 'Run next if input on'));

  // M17.2 Run next if output on
  Result.Add(NewCNCCode('M17.2 C', 'Run next if output on'));

  // M17.4 Run next if C = 1
  Result.Add(NewCNCCode('M17.4 C', 'Run next if C = 1 else skip'));

  // M18 Pause until input off
  Result.Add(NewCNCCode('M18 C', 'Pause until input off'));

  // M18.1 Run next if input on
  Result.Add(NewCNCCode('M18.1 C', 'Run next if input off'));

  // M18.2 Run next if output on
  Result.Add(NewCNCCode('M18.2 C', 'Run next if output off'));

  // M98 Run subprogram
  Result.Add(NewCNCCode('M98 ', 'Run subprogram'));

  // M99 End subprogram
  Result.Add(NewCNCCode('M99', 'End subprogram'));

  // M99.1 Abort
  Result.Add(NewCNCCode('M99.1', 'Abort'));
end;

end.

