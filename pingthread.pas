unit pingthread;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, pingsend, target, Dialogs;

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
var
  OldResult: Boolean;
begin
  try
    OldResult:= FTarget.LastLogEntry.Result;
    FTarget.LastLogEntry.Start := now;
    FTarget.LastLogEntry.Result := FPingCmd.Ping(FTarget.Address) and (FPingCmd.ReplyError = IE_NoError);
    FTarget.LastLogEntry.PingTime := FPingCmd.PingTime;

    if not FTarget.LastLogEntry.Result then
    begin
      FTarget.NumberOfErrors := FTarget.NumberOfErrors + 1;
    end;

    if not OldResult and not FTarget.LastLogEntry.Result then
    begin
      FTarget.CurrentErrors:= FTarget.CurrentErrors + 1;
    end
    else begin
      If not OldResult and FTarget.LastLogEntry.Result then
      begin
        FTarget.CurrentErrors:= 0;
        FTarget.WarningShown:= False;
      end;
    end;
  finally
    Synchronize(@ReturnTarget);
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
