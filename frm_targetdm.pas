unit frm_targetdm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, SQLite3Conn, SQLDB, target, Dialogs, DB;

type

  { TTargetData }

  TTargetData = class(TDataModule)
    SQLite3Connection: TSQLite3Connection;
    SQLQuery: TSQLQuery;
    SQLTransaction: TSQLTransaction;
  private
  public
    procedure CreateDatabase;
    procedure AddTarget(ATarget: string);
    procedure ReadTargets(var ATargetList: TTargetList);
    procedure ChangeTarget(ATarget: TTarget);
    procedure RemoveTarget(AID: integer);
    procedure AddLogEntry(ATarget: TTarget);
    function ReadLog(ATarget: TTarget; AAll: Boolean; ADate: TDate): String;
  end;

var
  TargetData: TTargetData;

implementation

{$R *.lfm}

{ TTargetData }

procedure TTargetData.CreateDatabase;
begin
  SQLite3Connection.Open;
  SQLTransaction.Active := True;

  SQLite3Connection.ExecuteDirect('CREATE TABLE tblTargets(id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT, target TEXT NOT NULL);');
  SQLite3Connection.ExecuteDirect('CREATE INDEX idxTargets ON tblTargets(id COLLATE NOCASE);');
  SQLite3Connection.ExecuteDirect('CREATE TABLE tblLog(ping_result BOOLEAN, ping_start DATETIME, ping_time INTEGER, target_id INTEGER NOT NULL, FOREIGN KEY (target_id) REFERENCES tblTargets (id));');

  SQLTransaction.Commit;
end;

procedure TTargetData.AddTarget(ATarget: string);
begin
  SQLQuery.Close;
  SQLQuery.SQL.Clear;

  SQLQuery.SQL.Text := 'INSERT INTO tblTargets VALUES(NULL, :Target);';
  SQLQuery.ParamByName('Target').AsString := ATarget;

  SQLQuery.ExecSQL;
  SQLTransaction.Commit;
end;

procedure TTargetData.ReadTargets(var ATargetList: TTargetList);
var
  i: integer;
  NewTarget: TTarget;
begin
  SQLQuery.Close;
  SQLQuery.SQL.Clear;

  SQLQuery.SQL.Text := 'SELECT id, target FROM tblTargets;';

  SQLQuery.Open;

  SQLQuery.First;
  for i := 0 to SQLQuery.RecordCount - 1 do
  begin
    NewTarget := TTarget.Create;
    NewTarget.Address := SQLQuery.FieldByName('target').AsString;
    NewTarget.ID := SQLQuery.FieldByName('id').AsInteger;

    ATargetList.Add(NewTarget);
    SQLQuery.Next;
  end;
end;

procedure TTargetData.ChangeTarget(ATarget: TTarget);
begin
  SQLQuery.Close;
  SQLQuery.SQL.Clear;

  SQLQuery.SQL.Text := 'UPDATE tblTargets SET target= :Target WHERE id= :TargetID;';
  SQLQuery.ParamByName('Target').AsString := ATarget.Address;
  SQLQuery.ParamByName('TargetID').AsInteger := ATarget.ID;

  SQLQuery.ExecSQL;
  SQLTransaction.Commit;
end;

procedure TTargetData.RemoveTarget(AID: integer);
begin
  SQLQuery.Close;
  SQLQuery.SQL.Clear;

  SQLQuery.SQL.Text := 'DELETE FROM tblTargets WHERE id= :TargetID;';
  SQLQuery.ParamByName('TargetID').AsInteger := AID;

  SQLQuery.ExecSQL;
  SQLTransaction.Commit;
end;

procedure TTargetData.AddLogEntry(ATarget: TTarget);
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

function TTargetData.ReadLog(ATarget: TTarget; AAll: Boolean; ADate: TDate): String;
var i: Integer;
    LastState: boolean;
begin
  SQLQuery.Close;
  SQLQuery.SQL.Clear;

  SQLQuery.SQL.Text := 'SELECT ping_result, ping_start, ping_time ' +
                       'FROM tblLog ' +
                       'WHERE target_id = :TargetID AND date(ping_start) = :PingStart;';
  SQLQuery.ParamByName('TargetID').asInteger := ATarget.ID;
  SQLQuery.ParamByName('PingStart').asString := FormatDateTime('YYYY"-"MM"-"DD', ADate);

  SQLQuery.Open;

  SQLQuery.First;
  Result:='';
  LastState := not SQLQuery.FieldByName('ping_result').AsBoolean;  //to print first event
  for i := 0 to SQLQuery.RecordCount - 1 do
  begin
    if (AAll) or (LastState <> SQLQuery.FieldByName('ping_result').AsBoolean) then
    begin
      Result += DateTimeToStr(SQLQuery.FieldByName('ping_start').AsDateTime) + ' - '  + BoolToStr(SQLQuery.FieldByName('ping_result').AsBoolean, 'OK', 'Fehler') + ' - ' + IntToStr(SQLQuery.FieldByName('ping_time').AsInteger) + 'ms' + LineEnding;
      LastState := SQLQuery.FieldByName('ping_result').AsBoolean;
    end;
    SQLQuery.Next;
  end;

end;

end.
