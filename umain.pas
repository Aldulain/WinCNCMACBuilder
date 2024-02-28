unit uMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, ActnList, PairSplitter, BCButton,
  BCSVGButton, uProcessEngine;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    psFunctionSplitter: TPairSplitter;
    pssFunctions: TPairSplitterSide;
    pssInputOutput: TPairSplitterSide;
    SaveAction: TAction;
    fileActionList: TActionList;
    btnProcDelete: TBCButton;
    btnProcNew: TBCButton;
    btnProcOpen: TBCButton;
    btnProcRefreshSelects: TBCButton;
    btnProcSave: TBCButton;
    btnMenuClose: TBCButton;
    edtProcName: TEdit;
    imLogoMenu: TImage;
    lbLMCodeSelect: TListBox;
    lbProcessSelect: TListBox;
    lbGCodeSelect: TListBox;
    lbInputSelect: TListBox;
    lbOutputSelect: TListBox;
    lblLMCodeSelectTtl: TLabel;
    lblGCodeSelectTtl: TLabel;
    lblInputSelectTtl: TLabel;
    lblProcName: TLabel;
    lblOutputSelectTtl: TLabel;
    lblProcSelectTtl: TLabel;
    memProcess: TMemo;
    odProcessOpen: TOpenDialog;
    pnlProcessBg: TPanel;
    pnlBG: TPanel;
    pnlFunctionFields: TPanel;
    pnlMACFields: TPanel;
    pnlLMCodeSelect: TPanel;
    pnlLMCodeSelectTtl: TPanel;
    pnlProcContainer: TPanel;
    pnlCfgProcesses: TPanel;
    pnlGCodeSelect: TPanel;
    pnlGCodeSelectTtl: TPanel;
    pnlInputSelect: TPanel;
    pnlInputSelectTtl: TPanel;
    pnlOutputSelect: TPanel;
    pnlOutputSelectTtl: TPanel;
    pnlProcSelect: TPanel;
    pnlProcSelectTtl: TPanel;
    pnlProcToolBar: TPanel;
    pnlContent: TPanel;
    pnlMenu: TPanel;
    sdProcessSave: TSaveDialog;
    sbProcesses: TScrollBox;
    procedure btnMenuCloseClick(Sender: TObject);
    procedure btnProcDeleteClick(Sender: TObject);
    procedure btnProcNewClick(Sender: TObject);
    procedure btnProcOpenClick(Sender: TObject);
    procedure btnProcRefreshSelectsClick(Sender: TObject);
    procedure btnProcSaveClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormShow(Sender: TObject);
    procedure lbGCodeSelectDblClick(Sender: TObject);
    procedure lbInputSelectDblClick(Sender: TObject);
    procedure lbLMCodeSelectDblClick(Sender: TObject);
    procedure lbOutputSelectDblClick(Sender: TObject);
    procedure lbProcessSelectDblClick(Sender: TObject);
    procedure memProcessChange(Sender: TObject);
    procedure memProcessEditingDone(Sender: TObject);
    procedure SaveActionExecute(Sender: TObject);
  private
    pProcessEngine: TProcessEngine;

    unsavedChanges : Boolean;

    procedure SetChangesMade(const bState : Boolean);
  public

  end;

var
  frmMain: TfrmMain;

implementation

uses
  StrUtils;

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.FormShow(Sender: TObject);
begin
  pProcessEngine := TProcessEngine.Create;
  pProcessEngine.RefreshSelects;

  sdProcessSave.InitialDir := GetCurrentDir;
  odProcessOpen.InitialDir := GetCurrentDir;

  SetChangesMade(false);
end;

procedure TfrmMain.lbGCodeSelectDblClick(Sender: TObject);
var
  sCodeCall : String;
  idx : Integer;
begin
  idx := lbGCodeSelect.ItemIndex;
  sCodeCall := pProcessEngine.GetGcodeTemplate(idx);
  memProcess.SelText := sCodeCall;
  memProcess.SetFocus;
  SetChangesMade(true);
end;

procedure TfrmMain.lbInputSelectDblClick(Sender: TObject);
var
  sInputCall : String;
  idx : Integer;
begin
  idx := lbInputSelect.ItemIndex;
  sInputCall := pProcessEngine.GetInputPort(idx);
  memProcess.SelText := sInputCall;
  memProcess.SetFocus;
  SetChangesMade(true);
end;

procedure TfrmMain.lbLMCodeSelectDblClick(Sender: TObject);
var
  sCodeCall : String;
  idx : Integer;
begin
  idx := lbLMCodeSelect.ItemIndex;
  sCodeCall := pProcessEngine.GetLMCodeTemplate(idx);
  memProcess.SelText := sCodeCall;
  memProcess.SetFocus;
  SetChangesMade(true);
end;

procedure TfrmMain.lbOutputSelectDblClick(Sender: TObject);
var
  sOutputCall : String;
  idx : Integer;
begin
  idx := lbOutputSelect.ItemIndex;
  sOutputCall := pProcessEngine.GetOutputPort(idx);
  memProcess.SelText := sOutputCall;
  memProcess.SetFocus;
  SetChangesMade(true);
end;

procedure TfrmMain.lbProcessSelectDblClick(Sender: TObject);
var
  sProgCall : String;
  idx : Integer;
begin
  idx := lbProcessSelect.ItemIndex;
  sProgCall := pProcessEngine.GetProgramLink(idx);
  memProcess.SelText := sProgCall;
  memProcess.SetFocus;
  SetChangesMade(true);
end;

procedure TfrmMain.memProcessChange(Sender: TObject);
begin
  SetChangesMade(true);
end;

procedure TfrmMain.memProcessEditingDone(Sender: TObject);
begin

end;

procedure TfrmMain.SaveActionExecute(Sender: TObject);
begin
  btnProcSaveClick(btnProcSave);
end;

procedure TfrmMain.SetChangesMade(const bState: Boolean);
begin
  unsavedChanges := bState;

  if bState then
    btnProcSave.Caption := 'Save *'
  else
    btnProcSave.Caption := 'Save';
end;

procedure TfrmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  pProcessEngine.Free;
end;

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if MessageDlg('Close Application', 'Exit Application?', mtConfirmation, mbYesNo, 0) = mrYes then
    CanClose := true
  else
    CanClose := false;
end;

procedure TfrmMain.btnProcRefreshSelectsClick(Sender: TObject);
begin
  if Assigned(pProcessEngine) then
    pProcessEngine.RefreshSelects;
end;

procedure TfrmMain.btnProcSaveClick(Sender: TObject);
var
  tfContent : TStringList;
  I : Integer;
begin
  tfContent := TStringList.Create;

  try
    sdProcessSave.FileName := edtProcName.Text;

    for I := 0 to memProcess.Lines.Count - 1 do
    begin
      tfContent.Append(memProcess.Lines[I]);
    end;

    if sdProcessSave.Execute then
    begin
      if Assigned(pProcessEngine) then
      begin
        pProcessEngine.SaveProgramToFile(sdProcessSave.FileName, tfContent);

        SetChangesMade(false);
      end;
    end;
  finally
    tfContent.Free;
  end;
end;

procedure TfrmMain.btnProcOpenClick(Sender: TObject);
var
  doOpen : TModalResult;
begin
  if (memProcess.Lines.Count > 0) and unsavedChanges then
  begin
    doOpen := MessageDlg('Open File', 'Do you want to open a different file?' + LineEnding + 'Unsaved changes will be lost.', mtConfirmation, mbYesNo, 0);
  end
  else
    doOpen := mrYes;

  if doOpen = mrYes then
  begin
    edtProcName.Text := '';
    memProcess.Clear;

    if odProcessOpen.Execute then
    begin
      if Assigned(pProcessEngine) then
        pProcessEngine.LoadProgramFromFile(ExtractFileName(odProcessOpen.FileName),odProcessOpen.FileName);
    end;

    SetChangesMade(false);
  end;
end;

procedure TfrmMain.btnProcNewClick(Sender: TObject);
var
  doNew : TModalResult;
begin
  if (memProcess.Lines.Count > 0) and unsavedChanges then
  begin
    doNew :=  MessageDlg('New File', 'Do you want to create a new file?' + LineEnding + 'Unsaved changes will be lost.', mtConfirmation, mbYesNo, 0);
  end
  else
    doNew := mrYes;

  if doNew = mrYes then
  begin
    edtProcName.Text := '';
    memProcess.Clear;
    SetChangesMade(false);
  end;
end;

procedure TfrmMain.btnProcDeleteClick(Sender: TObject);
begin
  if edtProcName.Text <> '' then
  begin
    if MessageDlg('Delete File', 'Do you want to delete the current file?' + LineEnding + 'This cannot be undone.', mtConfirmation, mbYesNo, 0) = mrYes then
      if Assigned(pProcessEngine) then
        pProcessEngine.DeleteProgramFile(edtProcName.Text);
  end;
end;

procedure TfrmMain.btnMenuCloseClick(Sender: TObject);
begin
  Close;
end;

end.

