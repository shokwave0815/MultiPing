unit pingthread;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, pingsend, target;

type

  TReturnTarget = procedure(ATartget: TTarget) of object;

  { TPingThread }

  TPingThread = class(TThread)
  private
    FOnFinish: TReturnTarget;
    FTarget: TTarget;
    FPingCmd: TPINGSend;
    procedure ReturnTarget;

  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: boolean; ATarget: TTarget);
    destructor Destroy; override;
    property OnFinish: TReturnTarget read FOnFinish write FOnFinish;
    property Target: TTarget read FTarget write FTarget;
  end;


implementation

{ TPingThread }

procedure TPingThread.ReturnTarget;
begin
  if Assigned(FOnFinish) then
  begin
    FOnFinish(FTarget);
  end;
end;

procedure TPingThread.Execute;
begin
  FTarget.LastLogEntry.Start := now;
  FTarget.LastLogEntry.Result := FPingCmd.Ping(FTarget.Address);
  FTarget.LastLogEntry.PingTime := FPingCmd.PingTime;
  if not FTarget.LastLogEntry.Result then
    FTarget.NumberOfErrors := FTarget.NumberOfErrors + 1;

  Synchronize(@ReturnTarget);
end;

constructor TPingThread.Create(CreateSuspended: boolean; ATarget: TTarget);
begin
  inherited Create(CreateSuspended);
  FreeOnTerminate := True;
  FTarget := ATarget;
  FPingCmd := TPINGSend.Create;
end;

destructor TPingThread.Destroy;
begin
  FreeAndNil(FPingCmd);
  inherited Destroy;
end;

end.
