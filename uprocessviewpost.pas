unit uProcessViewPost;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uCNCTypes;

type

  { TProcessViewPost }

  TProcessViewPost = class
  public
    procedure DoGCodePost(const aGCodes : TCNCCodeArray);
    procedure DoLMCodePost(const aLMCodes : TCNCCodeArray);
    procedure DoOutputPost(const aOutputs : TCNCIOArray);
    procedure DoInputPost(const aInputs : TCNCIOArray);
    procedure DoProgramPost(const aPrograms : TCNCProgramArray);
    procedure DoLoadProgram(const progName : String; const sLines : TStringList);
    procedure ClearProgram;
  end;

implementation

uses
  uMain;

{ TProcessViewPost }

procedure TProcessViewPost.DoGCodePost(const aGCodes: TCNCCodeArray);
var
  I : Integer;
begin
  frmMain.lbGCodeSelect.Clear;

  for I := Low(aGCodes) to High(aGCodes) do
  begin
    frmMain.lbGCodeSelect.Items.Add(aGCodes[I].GetDisplay);
  end;
end;

procedure TProcessViewPost.DoLMCodePost(const aLMCodes: TCNCCodeArray);
var
  I : Integer;
begin
  frmMain.lbLMCodeSelect.Clear;

  for I := Low(aLMCodes) to High(aLMCodes) do
  begin
    frmMain.lbLMCodeSelect.Items.Add(aLMCodes[I].GetDisplay);
  end;
end;

procedure TProcessViewPost.DoOutputPost(const aOutputs: TCNCIOArray);
var
  I : Integer;
begin
  frmMain.lbOutputSelect.Clear;

  for I := Low(aOutputs) to High(aOutputs) do
  begin
    frmMain.lbOutputSelect.Items.Add(aOutputs[I].GetDisplay);
  end;
end;

procedure TProcessViewPost.DoInputPost(const aInputs: TCNCIOArray);
var
  I : Integer;
begin
  frmMain.lbInputSelect.Clear;

  for I := Low(aInputs) to High(aInputs) do
  begin
    frmMain.lbInputSelect.Items.Add(aInputs[I].GetDisplay);
  end;
end;

procedure TProcessViewPost.DoProgramPost(const aPrograms: TCNCProgramArray);
var
  I : Integer;
begin
  frmMain.lbProcessSelect.Clear;

  for I := Low(aPrograms) to High(aPrograms) do
  begin
    frmMain.lbProcessSelect.Items.Add(aPrograms[I].name);
  end;
end;

procedure TProcessViewPost.DoLoadProgram(const progName: String; const sLines: TStringList);
var
  I : Integer;
begin
  frmMain.edtProcName.Text := progName;
  frmMain.memProcess.Clear;

  for I := 0 to sLines.Count - 1 do
  begin
    frmMain.memProcess.Append(sLines.Strings[I]);
  end;
end;

procedure TProcessViewPost.ClearProgram;
begin
  frmMain.edtProcName.Text := '';
  frmMain.memProcess.Clear;
end;

end.

