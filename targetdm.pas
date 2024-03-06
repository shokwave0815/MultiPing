unit targetdm;

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
    procedure RemoveTarget(AID: Integer);
  end;

var
  TargetData: TTargetData;

implementation

{$R *.lfm}

{ TTargetData }

procedure TTargetData.CreateDatabase;
begin
  SQLite3Connection.Open;
  SQLTransaction.Active:= True;

  SQLite3Connection.ExecuteDirect('CREATE TABLE tblTargets(ID Integer NOT NULL PRIMARY KEY AUTOINCREMENT, TARGET VCHAR(255));');
  SQLite3Connection.ExecuteDirect('CREATE INDEX idxTargets ON tblTargets(ID collate nocase);');

  SQLTransaction.Commit;
end;

procedure TTargetData.AddTarget(ATarget: string);
begin
  SQLQuery.Close;
  SQLQuery.SQL.Clear;

  SQLQuery.SQL.Text:= 'INSERT INTO tblTargets VALUES(NULL, :Target);';
  SQLQuery.ParamByName('Target').AsString := ATarget;

  SQLQuery.ExecSQL;
  SQLTransaction.Commit;
end;

procedure TTargetData.ReadTargets(var ATargetList: TTargetList);
var i: Integer;
    NewTarget: TTarget;
begin
  SQLQuery.Close;
  SQLQuery.SQL.Clear;

  SQLQuery.SQL.Text:= 'SELECT ID, TARGET FROM tblTargets;';

  SQLQuery.Open;

  SQLQuery.First;
  for i:= 0 to SQLQuery.RecordCount -1 do
  begin
    NewTarget := TTarget.Create;
    NewTarget.Adress := SQLQuery.FieldByName('TARGET').AsString;
    NewTarget.ID := SQLQuery.FieldByName('ID').AsInteger;

    ATargetList.Add(NewTarget);
    SQLQuery.Next;
  end;
end;

procedure TTargetData.RemoveTarget(AID: Integer);
begin
  SQLQuery.Close;
  SQLQuery.SQL.Clear;

  SQLQuery.SQL.Text:= 'DELETE FROM tblTargets WHERE ID= :TargetID;';
  SQLQuery.ParamByName('TargetID').AsInteger := AID;

  SQLQuery.ExecSQL;
  SQLTransaction.Commit;
end;

end.
