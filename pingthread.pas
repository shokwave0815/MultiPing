unit pingthread;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, pingsend, target;

type

  TOnFinish = procedure(ATartget: TTarget) of object;

  { TPingThread }

  TPingThread = class(TThread)
  private
    FOnFinish: TOnFinish;
    FTarget: TTarget;
    FPingCmd: TPINGSend;
    procedure CallOnFinish;
    procedure HandleErrors(const OldResult: Boolean);
    procedure ResetAlarm;
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: boolean; ATarget: TTarget);
    destructor Destroy; override;
    property OnFinish: TOnFinish read FOnFinish write FOnFinish;
  end;


implementation

{ TPingThread }

procedure TPingThread.CallOnFinish;
begin
  if Assigned(FOnFinish) then
  begin
    FOnFinish(FTarget);
  end;
end;

procedure TPingThread.HandleErrors(const OldResult: Boolean);
begin
  if not FTarget.CurrentLogEntry.Successful then
  begin
    FTarget.NumberOfErrors := FTarget.NumberOfErrors + 1;
  end;

  if not OldResult and not FTarget.CurrentLogEntry.Successful then
  begin
    FTarget.CurrentErrors:= FTarget.CurrentErrors + 1;
  end
  else begin
    if not OldResult and FTarget.CurrentLogEntry.Successful then
    begin
      ResetAlarm;
    end;
  end;
end;

procedure TPingThread.ResetAlarm;
begin
  FTarget.CurrentErrors:= 0;
  FTarget.WarningShown:= False;
end;

procedure TPingThread.Execute;
var
  OldResult: Boolean;
begin
  try
    OldResult:= FTarget.CurrentLogEntry.Successful;

    FTarget.CurrentLogEntry.Start := Now;
    FTarget.CurrentLogEntry.Successful := FPingCmd.Ping(FTarget.Address) and (FPingCmd.ReplyError = IE_NoError);
    FTarget.CurrentLogEntry.PingTime := FPingCmd.PingTime;

    HandleErrors(OldResult);
  finally
    Synchronize(@CallOnFinish);
  end;

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
