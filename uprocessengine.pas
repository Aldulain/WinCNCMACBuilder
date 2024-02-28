unit uProcessEngine;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uCNCGCodes, uCNCLMCodes, uCNCTypes, uProcessViewPost;

type

  { TProcessEngine }

  TProcessEngine = class
  private
    pViewPost : TProcessViewPost;
    pGCodes : TCNCCodeArray;
    pLMCodes : TCNCCodeArray;
    pOutputs : TCNCIOArray;
    pInputs : TCNCIOArray;
    pPrograms : TCNCProgramArray;

    pWinCNCIniPath : String;

    procedure SetWinCNCCodes;
    procedure LoadInputsAndOutputs;
    procedure LoadProcessFiles;
    procedure LocateWinCNCIni;
  public
    constructor Create; overload;
    destructor Destroy; override;
    procedure LoadProgramFromFile(const progName : String; const fileName : string);
    procedure SaveProgramToFile(const filePath : String; const sLines : TStringList);
    procedure DeleteProgramFile(const progName : String);
    procedure RefreshSelects;

    function GetGcodeTemplate(const idx : Integer): String;
    function GetLMCodeTemplate(const idx : Integer): String;
    function GetOutputPort(const idx : Integer): String;
    function GetInputPort(const idx : Integer): String;
    function GetProgramLink(const idx : Integer): String;

    property WinCNCIniPath : String Read pWinCNCIniPath write pWinCNCIniPath;
  end;

implementation

uses
  StrUtils;

{ TProcessEngine }

procedure TProcessEngine.SetWinCNCCodes;
begin
  pGCodes := BuildGCodeList;
  pLMCodes := BuildGenCNCCodes;
  pViewPost.DoGCodePost(pGCodes);
  pViewPost.DoLMCodePost(pLMCodes);

  LoadInputsAndOutputs;
  LoadProcessFiles;
end;

procedure TProcessEngine.LoadInputsAndOutputs;
var
  sFile : TStringList;
  sLine : String;
  iPos : Integer;
  i, j : Integer;
  ioString : String;
  ioChannel : String;
  ioComment : String;
  strChar : Char;
  inputFound, outputFound : Boolean;
begin
  sFile := TStringList.Create;

  try
    pInputs.Clear;
    pOutputs.Clear;

    if FileExists(pWinCNCIniPath) then
    begin
      sFile.LoadFromFile(pWinCNCIniPath);
      i := 0;

      try
        while i < sFile.Count do
        begin
          sLine := sFile[i];
          inputFound := false;
          outputFound := false;

          if Pos('auxin=', LowerCase(sLine)) > 0 then
            inputFound := true
          else if Pos('auxout=', LowerCase(sLine)) > 0 then
            outputFound := true;

          // check if input
          if inputFound or outputFound then
          begin
            ioChannel := '';
            ioComment := 'Undefined';
            // input found
            iPos := Pos('c', LowerCase(sLine));
            ioString := trim(RightStr(sLine, Length(sLine) - iPos));

            for j := 1 to Length(ioString) do
            begin
              if (ioString[j] in ['0'..'9']) then
                ioChannel += ioString[j]
              else
                break;
            end;

            iPos := Pos('[', ioString);
            if iPos > 0 then
              ioComment := trim(RightStr(ioString, Length(ioString) - iPos));

            if inputFound then
              pInputs.Add(NewCNCIO(StrToInt(ioChannel), ioComment))
            else if outputFound then
              pOutputs.Add(NewCNCIO(StrToInt(ioChannel), ioComment));
          end;

          Inc(I);
        end;
      except
        on E : Exception do
        begin
          // notify user
        end;
      end;
    end
    else
    begin
      pInputs.Add(NewCNCIO(-1, 'WinCNC.ini not found'));
      pOutputs.Add(NewCNCIO(-1, 'WinCNC.ini not found'));
    end;

    pViewPost.DoInputPost(pInputs);
    pViewPost.DoOutputPost(pOutputs);
  finally
    sFile.Free;
  end;
end;

procedure TProcessEngine.LoadProcessFiles;
begin
  pPrograms := LoadProgramsFromFile(GetCurrentDir);
  pViewPost.DoProgramPost(pPrograms);
end;

procedure TProcessEngine.LocateWinCNCIni;
var
  sFilePath : String;
begin
  sFilePath := GetCurrentDir + PathDelim + 'wincnc.ini';
  if FileExists(sFilePath) then
    pWinCNCIniPath := sFilePath
  else
  if FileExists('C:\WinCNC\wincnc.ini') then
    pWinCNCIniPath := 'C:\WinCNC\wincnc.ini';
end;

constructor TProcessEngine.Create;
begin
  pViewPost := TProcessViewPost.Create;
  LocateWinCNCIni;
end;

destructor TProcessEngine.Destroy;
begin
  pViewPost.Free;

  inherited Destroy;
end;

procedure TProcessEngine.LoadProgramFromFile(const progName: String; const fileName: string);
var
  tfContent : TStringList;
begin
  tfContent := TStringList.Create;

  try
    try
      tfContent.LoadFromFile(fileName);

      pViewPost.DoLoadProgram(progName, tfContent);
    except
      on E : Exception do
      begin
        // notify user
      end;
    end;
  finally
    tfContent.Free;
  end;
end;

procedure TProcessEngine.SaveProgramToFile(const filePath: String; const sLines: TStringList);
begin
  try
    sLines.SaveToFile(filePath);
    LoadProcessFiles;
  except
    on E : Exception do
    begin
      // notify user
    end;
  end;
end;

procedure TProcessEngine.DeleteProgramFile(const progName: String);
var
  sFilePath : String;
begin
  sFilePath := pPrograms.FindByName(progName).filepath;

  if sFilePath <> '' then
  begin
    try
      DeleteFile(sFilePath);
      pViewPost.ClearProgram;
      LoadProcessFiles;
    except
      on E : Exception do
      begin
        // notify user
      end;
    end;
  end;
end;

procedure TProcessEngine.RefreshSelects;
begin
  SetWinCNCCodes;
end;

function TProcessEngine.GetGcodeTemplate(const idx: Integer): String;
begin
  Result := pGCodes[idx].template;
end;

function TProcessEngine.GetLMCodeTemplate(const idx: Integer): String;
begin
  Result := pLMCodes[idx].template;
end;

function TProcessEngine.GetOutputPort(const idx: Integer): String;
begin
  Result := pOutputs[idx].port;
end;

function TProcessEngine.GetInputPort(const idx: Integer): String;
begin
  Result := pInputs[idx].port;
end;

function TProcessEngine.GetProgramLink(const idx: Integer): String;
begin
  Result := 'M98 ' + pPrograms[idx].name;
end;

end.

