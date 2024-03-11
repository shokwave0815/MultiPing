unit frm_targetdm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, SQLite3Conn, SQLDB, target, Dialogs, DB;

type

  { TTargetDatabase }

  TTargetDatabase = class(TDataModule)
    SQLite3Connection: TSQLite3Connection;
    SQLQuery: TSQLQuery;
    SQLTransaction: TSQLTransaction;
  private
    procedure SetFilename(AFilename: String);
    function GetFilename: String;
  public
    property Filename: String read GetFilename write SetFilename;
    procedure CreateDatabase;
    procedure ClearDatabase;
    procedure AddTarget(ATarget: string);
    procedure ReadTargets(var ATargetList: TTargetList);
    procedure ChangeTarget(ATarget: TTarget);
    procedure RemoveTarget(AID: integer);
    procedure AddLogEntry(ATarget: TTarget);
    function ReadLog(ATarget: TTarget; AAll: Boolean; AFiltered: Boolean; ADate: TDate): String;
    function ReadErrors(ATarget: TTarget): Integer;
  end;

var
  TargetDatabase: TTargetDatabase;

implementation

{$R *.lfm}

function CompareTargetsByPosition(const a, b: TTarget): Integer;
begin
  Result:= a.Position - b.Position;
end;

{ TTargetDatabase }

{*****************************************************************************
                                      Database
******************************************************************************}

procedure TTargetDatabase.SetFilename(AFilename: String);
begin
  SQLite3Connection.DatabaseName:= AFilename;
end;

function TTargetDatabase.GetFilename: String;
begin
  Result:= SQLite3Connection.DatabaseName;
end;

procedure TTargetDatabase.CreateDatabase;
begin
  SQLite3Connection.Open;
  SQLTransaction.Active := True;

  SQLite3Connection.ExecuteDirect('CREATE TABLE tblTargets(id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT, target TEXT NOT NULL, active BOOLEAN, position Integer);');
  SQLite3Connection.ExecuteDirect('CREATE INDEX idxTargets ON tblTargets(position COLLATE NOCASE);');
  SQLite3Connection.ExecuteDirect('CREATE TABLE tblLog(ping_result BOOLEAN, ping_start DATETIME, ping_time INTEGER, target_id INTEGER NOT NULL, FOREIGN KEY (target_id) REFERENCES tblTargets (id));');

  SQLTransaction.Commit;
end;

procedure TTargetDatabase.ClearDatabase;
begin
  SQLite3Connection.ExecuteDirect('End Transaction');
  SQLite3Connection.ExecuteDirect('VACUUM;');
  SQLite3Connection.ExecuteDirect('Begin Transaction');
end;

{*****************************************************************************
                                      Target
******************************************************************************}

procedure TTargetDatabase.AddTarget(ATarget: string);
begin
  SQLQuery.Close;
  SQLQuery.SQL.Clear;

  SQLQuery.SQL.Text := 'INSERT INTO tblTargets VALUES(NULL, :Target, :Active, :Position);';
  SQLQuery.ParamByName('Target').AsString := ATarget;
  SQLQuery.ParamByName('Active').AsBoolean := True;
  SQLQuery.ParamByName('Position').AsInteger := Integer.MaxValue;

  SQLQuery.ExecSQL;
  SQLTransaction.Commit;
end;

procedure TTargetDatabase.ReadTargets(var ATargetList: TTargetList);
var
  NewTarget: TTarget;
begin
  SQLQuery.Close;
  SQLQuery.SQL.Clear;

  SQLQuery.SQL.Text := 'SELECT id, target, active, position FROM tblTargets;';

  SQLQuery.Open;

  SQLQuery.First;
  while not SQLQuery.EOF do
  begin
    NewTarget := TTarget.Create;
    NewTarget.Address := SQLQuery.FieldByName('target').AsString;
    NewTarget.ID := SQLQuery.FieldByName('id').AsInteger;
    NewTarget.Active := SQLQuery.FieldByName('active').AsBoolean;
    NewTarget.Position := SQLQuery.FieldByName('position').AsInteger;

    ATargetList.Add(NewTarget);
    SQLQuery.Next;
  end;
  ATargetList.Sort(@CompareTargetsByPosition);
end;

procedure TTargetDatabase.ChangeTarget(ATarget: TTarget);
begin
  SQLQuery.Close;
  SQLQuery.SQL.Clear;

  SQLQuery.SQL.Text := 'UPDATE tblTargets ' +
                    'SET target= :Target, active = :Active, position = :Position ' +
                    'WHERE id= :TargetID;';
  SQLQuery.ParamByName('Target').AsString := ATarget.Address;
  SQLQuery.ParamByName('Active').AsBoolean := ATarget.Active;
  SQLQuery.ParamByName('Position').AsInteger := ATarget.Position;
  SQLQuery.ParamByName('TargetID').AsInteger := ATarget.ID;

  SQLQuery.ExecSQL;
  SQLTransaction.Commit;
end;

procedure TTargetDatabase.RemoveTarget(AID: integer);
begin
  SQLQuery.Close;
  SQLQuery.SQL.Clear;

  SQLQuery.SQL.Text := 'DELETE FROM tblLog WHERE target_id= :TargetID;';
  SQLQuery.ParamByName('TargetID').AsInteger := AID;

  SQLQuery.ExecSQL;
  SQLTransaction.Commit;

  SQLQuery.SQL.Text := 'DELETE FROM tblTargets WHERE id= :TargetID;';
  SQLQuery.ParamByName('TargetID').AsInteger := AID;

  SQLQuery.ExecSQL;
  SQLTransaction.Commit;

  ClearDatabase;
end;

{*****************************************************************************
                                      Log
******************************************************************************}

procedure TTargetDatabase.AddLogEntry(ATarget: TTarget);
begin
  SQLQuery.Close;
  SQLQuery.SQL.Clear;

  SQLQuery.SQL.Text := 'INSERT INTO tblLog VALUES(:PingResult, :PingStart, :PingTime, :TargetID);';
  SQLQuery.ParamByName('PingResult').AsBoolean := ATarget.LastLogEntry.Result;
  SQLQuery.ParamByName('PingStart').AsDateTime := ATarget.LastLogEntry.Start;
  SQLQuery.ParamByName('PingTime').AsInteger := ATarget.LastLogEntry.PingTime;
  SQLQuery.ParamByName('TargetID').AsInteger := ATarget.ID;

  SQLQuery.ExecSQL;
  SQLTransaction.Commit;
end;

function TTargetDatabase.ReadLog(ATarget: TTarget; AAll: Boolean; AFiltered: Boolean; ADate: TDate): String;
var LastState: boolean;
begin
  SQLQuery.Close;
  SQLQuery.SQL.Clear;
  if AFiltered then
  begin
    SQLQuery.SQL.Text := 'SELECT ping_result, ping_start, ping_time ' +
                         'FROM tblLog ' +
                         'WHERE target_id = :TargetID AND date(ping_start) = :PingStart;';
    SQLQuery.ParamByName('TargetID').asInteger := ATarget.ID;
    SQLQuery.ParamByName('PingStart').asString := FormatDateTime('YYYY"-"MM"-"DD', ADate);
  end else
  begin
    SQLQuery.SQL.Text := 'SELECT ping_result, ping_start, ping_time ' +
                         'FROM tblLog ' +
                         'WHERE target_id = :TargetID;';
    SQLQuery.ParamByName('TargetID').asInteger := ATarget.ID;
  end;

  SQLQuery.Open;

  SQLQuery.First;
  Result:='';
  LastState := not SQLQuery.FieldByName('ping_result').AsBoolean;  //to print first event
  while not SQLQuery.EOF do
  begin
    if (AAll) or (LastState <> SQLQuery.FieldByName('ping_result').AsBoolean) then
    begin
      if AFiltered then
      begin
        Result += TimeToStr(SQLQuery.FieldByName('ping_start').AsDateTime) + ' - '  + BoolToStr(SQLQuery.FieldByName('ping_result').AsBoolean, 'OK', 'Fehler') + ' - ' + IntToStr(SQLQuery.FieldByName('ping_time').AsInteger) + ' ms' + LineEnding;
      end else
      begin
        Result += DateTimeToStr(SQLQuery.FieldByName('ping_start').AsDateTime) + ' - '  + BoolToStr(SQLQuery.FieldByName('ping_result').AsBoolean, 'OK', 'Fehler') + ' - ' + IntToStr(SQLQuery.FieldByName('ping_time').AsInteger) + ' ms' + LineEnding;
      end;
      LastState := SQLQuery.FieldByName('ping_result').AsBoolean;
    end;
    SQLQuery.Next;
  end;

end;

function TTargetDatabase.ReadErrors(ATarget: TTarget): Integer;
begin
  SQLQuery.Close;
  SQLQuery.SQL.Clear;
  SQLQuery.SQL.Text := 'SELECT COUNT(ping_result) AS result ' +
                       'FROM tblLog ' +
                       'WHERE target_id = :TargetID AND date(ping_start) = :PingStart AND ping_result = FALSE;';
  SQLQuery.ParamByName('TargetID').asInteger := ATarget.ID;
  SQLQuery.ParamByName('PingStart').asString := FormatDateTime('YYYY"-"MM"-"DD', now);
  SQLQuery.Open;

  Result:= SQLQuery.FieldByName('result').AsInteger;
end;

end.
