unit uCNCTypes;

{$mode objfpc}{$H+}
{$modeswitch advancedRecords}
{$modeswitch typeHelpers}

interface

uses
  Classes, SysUtils;

type

  { TCNCCode }

  TCNCCode = record
    template : String;
    description : String;
  public
    function GetDisplay : String;
  end;

  { TCNCIO }

  TCNCIO = record
    port : String;
    comment : String;
  public
    function GetDisplay : String;
  end;

  TCNCCodeArray = array of TCNCCode;

  { CNCCodeArrayHelper }

  CNCCodeArrayHelper = type helper for TCNCCodeArray
    function Count : Integer;
    procedure Add(aCode : TCNCCode);
    procedure Clear;
    function Copy: TCNCCodeArray;
  end;

  TCNCIOArray = array of TCNCIO;

  { CNCIOArrayHelper }

  CNCIOArrayHelper = type helper for TCNCIOArray
    function Count : Integer;
    procedure Add(aIO : TCNCIO);
    procedure Clear;
    function Copy: TCNCIOArray;
  end;

  TCNCProgram = record
    filepath : String;
    name : String;
  end;

  TCNCProgramArray = array of TCNCProgram;

  { CNCProgramArrayHelper }

  CNCProgramArrayHelper = type helper for TCNCProgramArray
    function Count : Integer;
    procedure Add(aProgram : TCNCProgram);
    procedure Clear;
    function Copy: TCNCProgramArray;
    function FindByName(const progName : String) : TCNCProgram;
  end;

function NewCNCCode : TCNCCode;
function NewCNCCode(const aTemplate, aDescr: String): TCNCCode; overload;

function NewCNCIO(const aPort: Integer; const aComment : String) : TCNCIO;

function NewCNCProgram(const sName, sFilePath : String) : TCNCProgram;
function LoadProgramsFromFile(const fileDir : String) : TCNCProgramArray;

implementation

function NewCNCCode: TCNCCode;
begin
  result.template := '';
  result.description := '';
end;

function NewCNCCode(const aTemplate, aDescr: String): TCNCCode;
begin
  result.template := aTemplate;
  result.description := aDescr;
end;

function NewCNCIO(const aPort: Integer; const aComment: String): TCNCIO;
begin
  result.port := IntToStr(aPort);
  result.comment := aComment;
end;

function NewCNCProgram(const sName, sFilePath: String): TCNCProgram;
begin
  Result.name := sName;
  Result.filepath := sFilePath;
end;

function LoadProgramsFromFile(const fileDir: String): TCNCProgramArray;
var
  progSrch : TSearchRec;
  I : Integer;
  sName : String;
  sFilePath : String;
begin
  Result.Clear;

  try
    I := FindFirst(fileDir + PathDelim + '*.mac', faAnyFile, progSrch);
    while (I = 0) do
    begin
      sName := progSrch.Name;
      sFilePath := fileDir + PathDelim + progSrch.Name;

      Result.Add(NewCNCProgram(sName, sFilePath));

      I := FindNext(progSrch);
    end;
    FindClose(progSrch);
  except
    on E : Exception do
    begin
      FindClose(progSrch);
      // notify user
    end;
  end;
end;

{ CNCProgramArrayHelper }

function CNCProgramArrayHelper.Count: Integer;
begin
  result := Length(Self);
end;

procedure CNCProgramArrayHelper.Add(aProgram: TCNCProgram);
begin
  SetLength(Self, Length(Self) + 1);
  Self[High(Self)] := aProgram;
end;

procedure CNCProgramArrayHelper.Clear;
begin
  SetLength(Self, 0);
end;

function CNCProgramArrayHelper.Copy: TCNCProgramArray;
var
  I : Integer;
begin
  SetLength(Result, Length(Self));

  for I := Low(Self) to High(Self) do
  begin
    Result[I].filepath := Self[I].filepath;
    Result[I].name := Self[I].name;
  end;
end;

function CNCProgramArrayHelper.FindByName(const progName: String): TCNCProgram;
var
  I : Integer;
  found : Boolean;
begin
  for I := Low(Self) to High(Self) do
  begin
    if Self[I].name = progName then
    begin
      Result := Self[I];
      found := true;
      Break;
    end;
  end;

  if not Found then
    Result := NewCNCProgram('', '');
end;

{ CNCIOArrayHelper }

function CNCIOArrayHelper.Count: Integer;
begin
  result := Length(Self);
end;

procedure CNCIOArrayHelper.Add(aIO: TCNCIO);
begin
  SetLength(Self, Length(Self) + 1);
  Self[High(Self)] := aIO;
end;

procedure CNCIOArrayHelper.Clear;
begin
  SetLength(Self, 0);
end;

function CNCIOArrayHelper.Copy: TCNCIOArray;
var
  I : Integer;
begin
  SetLength(Result, Length(Self));

  for I := Low(Self) to High(Self) do
  begin
    Result[I].port := Self[I].port;
    Result[I].comment := Self[I].comment;
  end;
end;

{ TCNCIO }

function TCNCIO.GetDisplay: String;
begin
  result := 'C' + Self.port + ' - ' + trim(Self.comment);
end;

{ TCNCCode }

function TCNCCode.GetDisplay: String;
begin
  result := trim(Self.template) + ' - ' + trim(Self.description);
end;

{ CNCCodeArrayHelper }

function CNCCodeArrayHelper.Count: Integer;
begin
  result := Length(Self);
end;

procedure CNCCodeArrayHelper.Add(aCode: TCNCCode);
begin
  SetLength(Self, Length(Self) + 1);
  Self[High(Self)] := aCode;
end;

procedure CNCCodeArrayHelper.Clear;
begin
  SetLength(Self, 0);
end;

function CNCCodeArrayHelper.Copy: TCNCCodeArray;
var
  I : Integer;
begin
  SetLength(Result, Length(Self));

  for I := Low(Self) to High(Self) do
  begin
    Result[I].template := Self[I].template;
    Result[I].description := Self[I].description;
  end;
end;

end.

