unit configuration;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, frm_Log, INIFiles, Controls;

type

  { TConfiguration }

  TConfiguration = class(TObject)
  private
    FFilename: string;
    FForm: TForm;
    FINI: TIniFile;
    function ScaleFormTo96(ASize: integer): integer;
    function ScaleScreenTo96(ASize: integer): integer;
    function Scale96ToForm(ASize: integer): integer;
    function Scale96ToScreen(ASize: integer): integer;
  public
    property Filename: string read FFilename write FFilename;
    constructor Create(AFilename: string; var AMainForm: TForm);
    procedure Load;
    procedure Save;
  end;

implementation

uses frm_main;

  { TConfiguration }

function TConfiguration.ScaleFormTo96(ASize: integer): integer;
begin
  Result := FForm.ScaleFormTo96(ASize);
end;

function TConfiguration.ScaleScreenTo96(ASize: integer): integer;
begin
  Result := FForm.ScaleScreenTo96(ASize);
end;

function TConfiguration.Scale96ToForm(ASize: integer): integer;
begin
  Result := FForm.Scale96ToForm(ASize);
end;

function TConfiguration.Scale96ToScreen(ASize: integer): integer;
begin
  Result := FForm.Scale96ToScreen(ASize);
end;

constructor TConfiguration.Create(AFilename: string; var AMainForm: TForm);
begin
  inherited Create;
  FFilename := AFilename;
  FForm := AMainForm;
end;

procedure TConfiguration.Load;
var
  MainForm: TForm_Main;
begin
  if FForm <> NIL then
  begin
    MainForm := TForm_Main(FForm);
    FINI := TINIFile.Create(FFilename);

    //Dimension und Position der MainForm
    MainForm.Top :=         Scale96ToScreen(FINI.ReadInteger('Window', 'Top',    100));
    MainForm.Left :=        Scale96ToScreen(FINI.ReadInteger('Window', 'Left',   200));
    MainForm.Width :=       Scale96ToForm(FINI.ReadInteger(  'Window', 'Width',  800));
    MainForm.Height :=      Scale96ToForm(FINI.ReadInteger(  'Window', 'Height', 600));
    MainForm.WindowState := TWindowState(FINI.ReadInteger(   'Window', 'State',  0));
    MainForm.myHeight :=    MainForm.Height;
    MainForm.myWidth :=     MainForm.Width;

    Form_Log.Width :=  Scale96ToForm(FINI.ReadInteger('LogWindow', 'Width',  450));
    Form_Log.Height := Scale96ToForm(FINI.ReadInteger('LogWindow', 'Height', 400));

    //Größe der Spalten des StringGrid
    MainForm.StringGrid_Targets.Columns.Items[0].Width := Scale96ToForm(FINI.ReadInteger('SG', '0', 80));
    MainForm.StringGrid_Targets.Columns.Items[1].Width := Scale96ToForm(FINI.ReadInteger('SG', '1', 150));
    MainForm.StringGrid_Targets.Columns.Items[2].Width := Scale96ToForm(FINI.ReadInteger('SG', '2', 100));
    MainForm.StringGrid_Targets.Columns.Items[3].Width := Scale96ToForm(FINI.ReadInteger('SG', '3', 130));
    MainForm.StringGrid_Targets.Columns.Items[4].Width := Scale96ToForm(FINI.ReadInteger('SG', '4', 170));
    MainForm.StringGrid_Targets.Columns.Items[5].Width := Scale96ToForm(FINI.ReadInteger('SG', '5', 100));

    //Einstellungen
    MainForm.SpinEdit_Time.Value :=        FINI.ReadInteger('Prefs',  'Time',       30);
    Form_Log.CheckBox_AllEvents.Checked := FINI.ReadBool(   'Prefs',  'allEvents',  False);
    Form_Log.CheckBox_Filter.Checked :=    FINI.ReadBool(   'Prefs',  'Filtered',   True);
    Form_Log.DatePicker_Log.Enabled :=     Form_Log.CheckBox_Filter.Checked;
    MainForm.CheckBox_Alert.Checked :=     FINI.ReadBool('   Prefs',  'Alert',      False);
    MainForm.SpinEdit_Alert.Value :=       FINI.ReadInteger( 'Prefs', 'AlertCount', 3);

    FreeAndNil(FINI);
  end;
end;

procedure TConfiguration.Save;
var
  MainForm: TForm_Main;
begin
  if FForm <> NIL then
  begin

    MainForm := TForm_Main(FForm);
    FINI := TINIFile.Create(FFilename);

    //Dimension und Position der MainForm
    if (MainForm.WindowState = wsNormal) then //nicht speichern, wenn maximiert, minimiert
    begin
      FINI.WriteInteger('Window', 'Top',    ScaleScreenTo96(MainForm.Top));
      FINI.WriteInteger('Window', 'Left',   ScaleScreenTo96(MainForm.Left));
      FINI.WriteInteger('Window', 'Width',  ScaleFormTo96(MainForm.Width));
      FINI.WriteInteger('Window', 'Height', ScaleFormTo96(MainForm.Height));
    end;
    FINI.WriteInteger('Window', 'State', Ord(MainForm.WindowState));

    FINI.WriteInteger('LogWindow', 'Width',  ScaleFormTo96(Form_Log.Width));
    FINI.WriteInteger('LogWindow', 'Height', ScaleFormTo96(Form_Log.Height));

    //Größe der Spalten des StringGrid
    FINI.WriteInteger('SG', '0', ScaleFormTo96(MainForm.StringGrid_Targets.Columns.Items[0].Width));
    FINI.WriteInteger('SG', '1', ScaleFormTo96(MainForm.StringGrid_Targets.Columns.Items[1].Width));
    FINI.WriteInteger('SG', '2', ScaleFormTo96(MainForm.StringGrid_Targets.Columns.Items[2].Width));
    FINI.WriteInteger('SG', '3', ScaleFormTo96(MainForm.StringGrid_Targets.Columns.Items[3].Width));
    FINI.WriteInteger('SG', '4', ScaleFormTo96(MainForm.StringGrid_Targets.Columns.Items[4].Width));
    FINI.WriteInteger('SG', '5', ScaleFormTo96(MainForm.StringGrid_Targets.Columns.Items[5].Width));

    //Einstellungen
    FINI.WriteBool('Prefs',    'allEvents',  Form_Log.CheckBox_AllEvents.Checked);
    FINI.WriteBool('Prefs',    'Filtered',   Form_Log.CheckBox_Filter.Checked);
    FINI.WriteInteger('Prefs', 'Time',       MainForm.SpinEdit_Time.Value);
    FINI.WriteBool('Prefs',    'Alert',      MainForm.CheckBox_Alert.Checked);
    FINI.WriteInteger('Prefs', 'AlertCount', MainForm.SpinEdit_Alert.Value);

    FreeAndNil(FINI);

  end;

end;

end.
