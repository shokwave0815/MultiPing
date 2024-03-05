unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Grids, Spin, pingsend, target, fgl, frm_details, LCLType, Types, INIFiles;

type

  TTargetList = specialize TFPGObjectList<TTarget>;

  { TForm_Main }

  TForm_Main = class(TForm)
    Btn_Start: TButton;
    Btn_AddTarget: TButton;
    Btn_Delete: TButton;
    Btn_Details: TButton;
    ChBo_AllEvents: TCheckBox;
    Edit_AddTarget: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Panel1: TPanel;
    SG_Targets: TStringGrid;
    SE_Time: TSpinEdit;
    Timer: TTimer;
    procedure Btn_AddTargetClick(Sender: TObject);
    procedure Btn_DetailsClick(Sender: TObject);
    procedure Btn_StartClick(Sender: TObject);
    procedure Btn_DeleteClick(Sender: TObject);
    procedure Edit_AddTargetKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormWindowStateChange(Sender: TObject);
    procedure SE_TimeKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure SG_TargetsDrawCell(Sender: TObject; aCol, aRow: integer; aRect: TRect; aState: TGridDrawState);
    procedure TimerStartTimer(Sender: TObject);
    procedure TimerStopTimer(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    isStartup: Boolean;
    AppDir: String;
    cfgIni: TIniFile;
    myHeight, myWidth: Integer;
    PingCmd: TPINGSend;
    TargetList: TTargetList;
    procedure PingTarget(ATarget: TTarget);
    procedure ClearGrid;
    procedure PrintTargets;
    procedure AddTarget(Target: string);
    procedure LoadTargets;
    procedure SaveTargets;
    procedure LoadConfig;
    procedure SaveConfig;
  public

  end;

var
  Form_Main: TForm_Main;

implementation

{$R *.lfm}

{ TForm_Main }

procedure TForm_Main.Btn_StartClick(Sender: TObject);
begin
  if not Timer.Enabled then
    Timer.Interval := SE_Time.Value * 1000;
  Timer.Enabled := not Timer.Enabled;
end;

procedure TForm_Main.Btn_DeleteClick(Sender: TObject);
begin
  if SG_Targets.Row > 0 then
  begin
    if MessageDlg('Das Ziel "' + TargetList.Items[SG_Targets.Row - 1].Adress + '" wirklich löschen?',
      mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      TargetList.Delete(SG_Targets.Row - 1);
      SG_Targets.RowCount:= SG_Targets.RowCount - 1;
      PrintTargets;
    end;
  end;
end;

procedure TForm_Main.Edit_AddTargetKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  Shift:=Shift;;
  if key = VK_RETURN then
  begin
    Btn_AddTarget.Click;
  end;
end;

procedure TForm_Main.Btn_AddTargetClick(Sender: TObject);
begin
  if Trim(Edit_AddTarget.Text) <> '' then
  begin
    AddTarget(Edit_AddTarget.Text);
    Edit_AddTarget.Text := '';
    SelectNext(Btn_Start, True, True);
    SG_Targets.RowCount:= SG_Targets.RowCount + 1;
    PrintTargets;
  end;
end;

procedure TForm_Main.Btn_DetailsClick(Sender: TObject);
var
  i: Integer;
  LastState: Boolean;
  DetailsList: TDetailsList;
begin
  if (SG_Targets.Row > 0) and (TargetList[SG_Targets.Row - 1].History.Count > 0) then
  begin
    Form_Details.Memo1.Clear;
    DetailsList := TargetList[SG_Targets.Row - 1].History;
    Form_Details.Caption:= 'Details für ' + TargetList[SG_Targets.Row - 1].Adress;
    LastState:= not DetailsList.First.Result;  //to print first event
    for i := 0 to DetailsList.Count - 1 do
    begin
      if (ChBo_AllEvents.Checked) or (LastState <> DetailsList.Items[i].Result) then
      begin
        Form_Details.Memo1.Append(DateTimeToStr(DetailsList.Items[i].Time) + ' - ' +
          BoolToStr(DetailsList.Items[i].Result, 'OK', 'Fehler') + ' - ' +
          IntToStr(DetailsList.Items[i].PingTime) + 'ms');
        LastState:= DetailsList.Items[i].Result;
      end;
    end;
    Form_Details.Show;
  end;
end;

procedure TForm_Main.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Timer.Enabled:= False;

  SaveConfig;
  SaveTargets;

  FreeAndNil(TargetList);
  FreeAndNil(PingCmd);

  CloseAction := caFree;
end;

procedure TForm_Main.FormCreate(Sender: TObject);
begin
  isStartup:= true;
  AppDir := ExtractFilePath(ParamStr(0));
  PingCmd := TPINGSend.Create;
  TargetList := TTargetList.Create;
end;

procedure TForm_Main.FormResize(Sender: TObject);
begin
  if WindowState = wsNormal then
  begin
    myHeight:= Height;
    myWidth:= Width;
  end;
end;

procedure TForm_Main.FormShow(Sender: TObject);
begin
  if isStartup then
  begin
    LoadConfig;
    LoadTargets;
    PrintTargets;
    isStartup:= false;
  end;
end;

procedure TForm_Main.FormWindowStateChange(Sender: TObject);
begin
  if WindowState = wsNormal then
  begin
    Sleep(10); //ohne sleep reduziert sich das Fenster zur Unkenntlichkeit
    Height := myHeight;
    Width := myWidth;
  end;
end;

procedure TForm_Main.SE_TimeKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  Shift:= Shift;
  if key = VK_RETURN then
  begin
    Btn_Start.Click;
  end;
end;

procedure TForm_Main.SG_TargetsDrawCell(Sender: TObject; aCol, aRow: integer; aRect: TRect; aState: TGridDrawState);
begin
  if not (gdFixed in aState) and not (gdSelected in aState) and Timer.Enabled then
  begin
    case SG_Targets.Cells[1, aRow] of
      'ja': begin
        SG_Targets.canvas.Brush.Color := TColor($80CC80);
      end;
      'nein': begin
        SG_Targets.canvas.Brush.Color := TColor($A0A0FF);
      end;
      else
      begin
        SG_Targets.Canvas.Font.Color := clBlack;
      end;
    end;

    SG_Targets.Canvas.FillRect(arect);
    SG_Targets.Canvas.TextOut(aRect.Left + 2, aRect.Top + 2, SG_Targets.Cells[aCol, aRow]);
    SG_Targets.Canvas.FrameRect(aRect);
  end;
end;

procedure TForm_Main.TimerStartTimer(Sender: TObject);
begin
  SE_Time.Enabled:= False;
  Btn_Start.Caption := 'Stop';
  TimerTimer(Sender);
end;

procedure TForm_Main.TimerStopTimer(Sender: TObject);
begin
  Btn_Start.Caption := 'Start';
  SE_Time.Enabled:= True;
  PrintTargets;
end;

procedure TForm_Main.TimerTimer(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to TargetList.Count - 1 do
  begin
    PingTarget(TargetList.Items[i]);
  end;
  PrintTargets;
end;

procedure TForm_Main.PingTarget(ATarget: TTarget);
var
  Details: TDetails;
begin
  Details := TDetails.Create;

  Details.Result := PingCmd.Ping(ATarget.Adress);
  Details.Time := now;
  Details.PingTime := PingCmd.PingTime;
  Details.Interval:= Timer.Interval;

  ATarget.History.Add(Details);
end;

procedure TForm_Main.ClearGrid;
begin
  SG_Targets.RowCount := 1;
  SG_Targets.RowCount := TargetList.Count + 1;
end;

procedure TForm_Main.PrintTargets;
var
  i: integer;
begin
  //ClearGrid;
  for i := 0 to TargetList.Count - 1 do
  begin
    SG_Targets.Cells[0, i + 1] := TargetList[i].Adress;
    if TargetList[i].History.Count > 0 then
    begin
      SG_Targets.Cells[1, i + 1] := BoolToStr(TargetList[i].History.Last.Result, 'ja', 'nein');
      SG_Targets.Cells[2, i + 1] := IntToStr(TargetList[i].History.Last.PingTime) + 'ms';
      SG_Targets.Cells[3, i + 1] := DateTimeToStr(TargetList[i].History.Last.Time);
    end;
  end;
end;

procedure TForm_Main.AddTarget(Target: string);
var
  NewTarget: TTarget;
begin
  if Trim(Target) <> '' then
  begin
    NewTarget := TTarget.Create;
    NewTarget.Adress := Target;
    TargetList.Add(NewTarget);
  end;
end;

procedure TForm_Main.LoadTargets;
var
  sl: TStringList;
  i: integer;
begin
  if FileExists(AppDir + 'targets.txt') then
  begin
    sl := TStringList.Create;
    sl.LoadFromFile(AppDir + 'targets.txt');
    SG_Targets.RowCount:= sl.Count + 1;
    for i := 0 to sl.Count - 1 do
    begin
      AddTarget(sl[i]);
    end;
    FreeAndNil(sl);
  end;
end;

procedure TForm_Main.SaveTargets;
var
  sl: TStringList;
  i: integer;
begin
  sl := TStringList.Create;
  for i := 0 to TargetList.Count - 1 do
  begin
    sl.Append(TargetList.Items[i].Adress);
  end;
  sl.SaveToFile(AppDir + 'targets.txt');
  FreeAndNil(sl);
end;

procedure TForm_Main.LoadConfig;
begin
  cfgINI := TINIFile.Create(AppDir + 'config.ini');

  //Dimension und Position der MainForm
  Top := Scale96ToScreen(cfgINI.ReadInteger('Window', 'Top', 100));
  Left := Scale96ToScreen(cfgINI.ReadInteger('Window', 'Left', 200));
  Width := Scale96ToForm(cfgINI.ReadInteger('Window', 'Width', 800));
  Height := Scale96ToForm(cfgINI.ReadInteger('Window', 'Height', 600));
  WindowState := TWindowState(cfgINI.ReadInteger('Window', 'State', 0));
  myHeight := Height;
  myWidth := Width;

  //Größe der Spalten des StringGrid
  SG_Targets.Columns.Items[0].Width := Scale96ToForm(cfgINI.ReadInteger('SG', '0', 300));
  SG_Targets.Columns.Items[1].Width := Scale96ToForm(cfgINI.ReadInteger('SG', '1', 150));
  SG_Targets.Columns.Items[2].Width := Scale96ToForm(cfgINI.ReadInteger('SG', '2', 130));
  SG_Targets.Columns.Items[3].Width := Scale96ToForm(cfgINI.ReadInteger('SG', '3', 200));

  //Einstellungen
  SE_Time.Value:= cfgINI.ReadInteger('Prefs', 'Time', 30);
  ChBo_AllEvents.Checked:= cfgIni.ReadBool('Prefs', 'allEvents', False);

  FreeAndNil(CfgINI);
end;

procedure TForm_Main.SaveConfig;
begin
  cfgINI := TINIFile.Create(AppDir + 'config.ini');

  //Dimension und Position der MainForm
  if (WindowState = wsNormal) then //nicht speichern, wenn maximiert, minimiert
  begin
    cfgINI.WriteInteger('Window', 'Top', ScaleScreenTo96(Top));
    cfgINI.WriteInteger('Window', 'Left', ScaleScreenTo96(Left));
    cfgINI.WriteInteger('Window', 'Width', ScaleFormTo96(Width));
    cfgINI.WriteInteger('Window', 'Height', ScaleFormTo96(Height));
  end;
  cfgINI.WriteInteger('Window', 'State', Ord(WindowState));

  //Größe der Spalten des StringGrid
  cfgINI.WriteInteger('SG', '0', ScaleFormTo96(SG_Targets.Columns.Items[0].Width));
  cfgINI.WriteInteger('SG', '1', ScaleFormTo96(SG_Targets.Columns.Items[1].Width));
  cfgINI.WriteInteger('SG', '2', ScaleFormTo96(SG_Targets.Columns.Items[2].Width));
  cfgINI.WriteInteger('SG', '3', ScaleFormTo96(SG_Targets.Columns.Items[3].Width));

  //Einstellungen
  cfgIni.WriteInteger('Prefs', 'Time', SE_Time.Value);
  cfgIni.WriteBool('Prefs', 'allEvents', ChBo_AllEvents.Checked);
  FreeAndNil(cfgINI);
end;

end.
