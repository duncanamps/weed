program weed;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp
  { you can add units after this }   ;


const
  ARRAY_INCREMENT          = 256;
  DEFAULT_DAYS_TO_KEEP     = 14;
  DEFAULT_WEEKS_TO_KEEP    = 13;
  DEFAULT_MONTHS_TO_KEEP   = 12;
  DEFAULT_QUARTERS_TO_KEEP = 8;
  DEFAULT_YEAR             = 2000;
  EPOCH_YEAR               = 1990;
  OPTIONS_SHORT : string = 'd:efhm:nqvw:';
  OPTIONS_LONG : array[0..8] of string = ('days:','debug','filedate','help','months:','noconfirm','quarters:','verbose','weeks:');

type
  TParticle = (tpYear,tpMonth,tpDay,tpHour,tpMinute,tpSecond);

  TParticleArray = array[TParticle] of integer;

  TWeedStatus = (wsUnknown,wsDaily,wsWeekly,wsMonthly,wsQuarterly,wsYearly,wsDelete);

  TWeedFile = record
    Filename: string;
    Filedate: TDateTime;
    Status:   TWeedStatus;
  end;

  TWeedArray = array of TWeedFile;

  { TWeed }

  TWeed = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    ConfirmDeletes: boolean;
    DaysToKeep:     integer;
    Debug:          boolean;
    Indexes:        TParticleArray;
    Lengths:        TParticleArray;
    MonthsToKeep:   integer;
    QuartersToKeep: integer;
    UseFileDate:    boolean;
    Values:         TParticleArray;
    Verbose:        boolean;
    WeedArray:      TWeedArray;
    WeeksToKeep:    integer;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure ProcessFileSpec(FileSpec: string);
    procedure ProcessNumericOption(ShortSwitch: char; LongSwitch: string; var variable: integer; lowlimit: integer; highlimit: integer);
    procedure ProcessParticle(var spec: string; const particle: string; const replacement: string; var index: integer; var len: integer);
    procedure SortWeedArray;
    procedure WriteBasicHelp;
    procedure WriteHelp; virtual;
  end;

{ TWeed }

procedure TWeed.DoRun;
var
  ErrorMsg: String;
  NonOptions: TStringArray;
  i:          integer;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions(OPTIONS_SHORT,OPTIONS_LONG);
  if ErrorMsg<>'' then
    raise Exception.Create(ErrorMsg);


  // Check for no command line
  if ParamCount = 0 then
    begin
      WriteBasicHelp;
      Terminate(0);
      Exit;
    end;

  // parse parameters
  if HasOption('h', 'help') then begin
    WriteHelp;
    Terminate(0);
    Exit;
  end;

  { add your program here }
  if not Terminated then
    begin
      { Initialise }
      DaysToKeep     := DEFAULT_DAYS_TO_KEEP;
      WeeksToKeep    := DEFAULT_WEEKS_TO_KEEP;
      MonthsToKeep   := DEFAULT_MONTHS_TO_KEEP;
      QuartersToKeep := DEFAULT_QUARTERS_TO_KEEP;
      ConfirmDeletes := true;
      Debug          := false;
      UseFileDate    := false;
      Verbose        := false;

      { Process command line options }
      if HasOption('e','debug') then
        Debug := true;
      if HasOption('f','filedate') then
        UseFileDate := true;
      if HasOption('n','noconfirm') then
        ConfirmDeletes := false;
      if HasOption('v','verbose') then
        Verbose := true;
      ProcessNumericOption('d','days',    DaysToKeep,     1,  366);
      ProcessNumericOption('w','weeks',   WeeksToKeep,    1,  104);
      ProcessNumericOption('m','months',  MonthsToKeep,   1,99999);
      ProcessNumericOption('q','quarters',QuartersToKeep, 1,99999);

      { Process command line filename }

      NonOptions := GetNonOptions(OPTIONS_SHORT,OPTIONS_LONG);

      { Show some preamble info if verbose is switched on }
      if (not Terminated and Verbose) then
        begin
          writeln('Confirm deletes  = ' + BoolToStr(ConfirmDeletes,true));
          writeln('Use file date    = ' + BoolToStr(UseFileDate,true));
          writeln('Verbose          = ' + BoolToStr(Verbose,true)); // Bit pointless as this will always be true!
          writeln('Days to keep     = ' + IntToStr(DaysToKeep));
          writeln('Weeks to keep    = ' + IntToStr(WeeksToKeep));
          writeln('Months to keep   = ' + IntToStr(MonthsToKeep));
          writeln('Quarters to keep = ' + IntToStr(QuartersToKeep));
          if Length(NonOptions) = 0 then
            writeln('No filespec specified')
          else
            for i := Low(NonOptions) to High(NonOptions) do
              begin
                writeln('Filespec ' + Format('%3d',[i+1]) + '    = ' + NonOptions[i]);
              end;
        end;

      { Process each filespec }
      if Length(NonOptions) > 0 then
        for i := Low(NonOptions) to High(NonOptions) do
          ProcessFilespec(NonOptions[i]);
    end;

  // stop program loop
  Terminate;
end;

constructor TWeed.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TWeed.Destroy;
begin
  inherited Destroy;
end;


{ Process a particle within the filespec }

procedure TWeed.ProcessParticle(var spec: string; const particle: string; const replacement: string; var index: integer; var len: integer);
begin
  if Debug then
    WriteLn('Processing particle ' + particle);
  if Pos(particle,spec) > 0 then
    begin
      index := Pos(particle,spec);
      len   := Length(particle) - 1;
      if Debug then
        writeln('    Particle found at index ' + IntToStr(index) + ' in ' + spec);
      Delete(spec,index,Length(particle));
      Insert(replacement,spec,index);
      if Debug then
        writeln('    New spec is ' + spec);
    end;
end;


{ Process an individual file specification }

procedure TWeed.ProcessFileSpec(FileSpec: string);
var SearchSpec:     string;
    Info:           TSearchRec;
    i:              TParticle;
    Count:          integer;
    j:              integer;
    date_now:       TDateTime;
    date_search:    TDateTime;
    date_limit:     TDateTime;
    y,m,d:          word;
    delete_flag:    boolean;
    input:          string;
begin
  writeln('PROCESSING filespec ' + FileSpec);
  SearchSpec := FileSpec;
  // Particle processing
  for i := Low(TParticle) to High(TParticle) do
    Indexes[i] := -1;
  ProcessParticle(SearchSpec,'$YYYY','????',Indexes[tpYear],   Lengths[tpYear]);
  ProcessParticle(SearchSpec,'$YY',  '??',  Indexes[tpYear],   Lengths[tpYear]);
  ProcessParticle(SearchSpec,'$MM',  '??',  Indexes[tpMonth],  Lengths[tpMonth]);
  ProcessParticle(SearchSpec,'$DD',  '??',  Indexes[tpDay],    Lengths[tpDay]);
  ProcessParticle(SearchSpec,'$HH',  '??',  Indexes[tpHour],   Lengths[tpHour]);
  ProcessParticle(SearchSpec,'$NN',  '??',  Indexes[tpMinute], Lengths[tpMinute]);
  ProcessParticle(SearchSpec,'$SS',  '??',  Indexes[tpSecond], Lengths[tpSecond]);
  // Now search for files
  if Debug then
    writeln('SearchSpec = ' + SearchSpec);
  SetLength(WeedArray,0);
  Count := 0;
  if FindFirst(SearchSpec,0,Info) = 0 then
    repeat
      if Length(WeedArray) <= Count then             // Resize array if reqd
        SetLength(WeedArray,Count + ARRAY_INCREMENT);
      WeedArray[Count].Filename := ExtractFilePath(SearchSpec) + Info.Name;
      WeedArray[Count].Filedate := EncodeDate(DEFAULT_YEAR,1,1);
      WeedArray[Count].Status   := wsUnknown;
      if Debug then
        writeln('Found file ' + WeedArray[Count].Filename);
      if UseFileDate then
        WeedArray[Count].Filedate := FileDateToDateTime(Info.Time)
      else
        begin // Parse the date out of the filename
          Values[tpYear]   := DEFAULT_YEAR;
          Values[tpMonth]  := 1;
          Values[tpDay]    := 1;
          Values[tpHour]   := 0;
          Values[tpMinute] := 0;
          values[tpSecond] := 0;
          for i := Low(TParticle) to High(TParticle) do
            if Indexes[i] > 0 then
              begin
                Values[i] := StrToInt(Copy(WeedArray[Count].Filename,Indexes[i],Lengths[i]));
                if (i = tpYear) and (Values[i] < 100) then
                  begin
                    Values[i] := Values[i] + 2000;
                    if Values[i] >= EPOCH_YEAR + 100 then
                      Values[i] := Values[i] - 100;
                  end;
              end;
          WeedArray[Count].Filedate := ComposeDateTime(EncodeDate(Values[tpYear],Values[tpMonth],Values[tpDay]),
                                                       EncodeTime(Values[tpHour],Values[tpMinute],Values[tpSecond],0));
        end;
      if Debug then
        writeln('    Using date/time of ' + FormatDateTime('yyyy/mm/dd hh:nn:ss',WeedArray[Count].Filedate));
      Inc(Count);
    until FindNext(Info) <> 0;
  SetLength(WeedArray,Count);

  { Sort the array }
  SortWeedArray;

  { Process annual files }
  if Verbose then
    writeln('Processing annual files');
  date_now := Now();
  DecodeDate(date_now,y,m,d);
  m := 1;
  d := 1;
  date_search := ComposeDateTime(EncodeDate(y,m,d),
                                 EncodeTime(0,0,0,0));
  if Debug then
    writeln('    Date search is ' + FormatDateTime('yyyy/mm/dd hh:nn',date_search));
  for j := 0 to Length(WeedArray) - 1 do
    if WeedArray[j].Filedate < date_search then
      begin
        WeedArray[j].Status := wsYearly;
        if Verbose then
          writeln('    ' + WeedArray[j].Filename + ' assigned to YEARLY');
        Dec(y);
        date_search := ComposeDateTime(EncodeDate(y,m,d),
                                       EncodeTime(0,0,0,0));
        if Debug then
          writeln('    Date search is ' + FormatDateTime('yyyy/mm/dd hh:nn',date_search));
      end;

  { Process quarterly files }

  if Verbose then
    writeln('Processing quarterly files');
  date_limit := date_now - 365.25 * QuartersToKeep / 4.0;
  DecodeDate(date_now,y,m,d);
  d := 1;
  while not (m in [1,4,7,10]) do
    begin
      Dec(m);
      if m < 1 then
        begin
          m := 12;
          Dec(y);
        end;
    end;
  date_search := ComposeDateTime(EncodeDate(y,m,d),
                                 EncodeTime(0,0,0,0));
  if Debug then
    begin
      writeln('    Date limit  is ' + FormatDateTime('yyyy/mm/dd hh:nn',date_limit));
      writeln('    Date search is ' + FormatDateTime('yyyy/mm/dd hh:nn',date_search));
    end;
  for j := 0 to Length(WeedArray) - 1 do
    if (WeedArray[j].Filedate < date_search) and (WeedArray[j].Filedate >= date_limit) then
      begin
        WeedArray[j].Status := wsQuarterly;
        if Verbose then
          writeln('    ' + WeedArray[j].Filename + ' assigned to QUARTERLY');
        DecodeDate(WeedArray[j].Filedate,y,m,d);
        d := 1;
        while not (m in [1,4,7,10]) do
          begin
            Dec(m);
            if m < 1 then
              begin
                m := 12;
                Dec(y);
              end;
          end;
        date_search := ComposeDateTime(EncodeDate(y,m,d),
                                       EncodeTime(0,0,0,0));
        if Debug then
          writeln('    Date search is ' + FormatDateTime('yyyy/mm/dd hh:nn',date_search));
      end;

  { Process monthly files  }

  if Verbose then
    writeln('Processing monthly files');
  date_limit := date_now - 365.25 * MonthsToKeep / 12.0;
  DecodeDate(date_now,y,m,d);
  d := 1;
  date_search := ComposeDateTime(EncodeDate(y,m,d),
                                 EncodeTime(0,0,0,0));
  if Debug then
    begin
      writeln('    Date limit  is ' + FormatDateTime('yyyy/mm/dd hh:nn',date_limit));
      writeln('    Date search is ' + FormatDateTime('yyyy/mm/dd hh:nn',date_search));
    end;
  for j := 0 to Length(WeedArray) - 1 do
    if (WeedArray[j].Filedate < date_search) and (WeedArray[j].Filedate >= date_limit) then
      begin
        WeedArray[j].Status := wsMonthly;
        if Verbose then
          writeln('    ' + WeedArray[j].Filename + ' assigned to MONTHLY');
        DecodeDate(WeedArray[j].Filedate,y,m,d);
        d := 1;
        date_search := ComposeDateTime(EncodeDate(y,m,d),
                                       EncodeTime(0,0,0,0));
        if Debug then
          writeln('    Date search is ' + FormatDateTime('yyyy/mm/dd hh:nn',date_search));
      end;

  { Process weekly files  }
  if Verbose then
    writeln('Processing weekly files');
  date_limit := date_now - 7.0 * WeeksToKeep;
  date_search := Trunc(date_now);
  while DayOfWeek(date_search) > 1 do // Bump down to Sunday
    date_search := date_search - 1.0;
  if Debug then
    begin
      writeln('    Date limit  is ' + FormatDateTime('yyyy/mm/dd hh:nn',date_limit));
      writeln('    Date search is ' + FormatDateTime('yyyy/mm/dd hh:nn',date_search));
    end;
  for j := 0 to Length(WeedArray) - 1 do
    if (WeedArray[j].Filedate < date_search) and (WeedArray[j].Filedate >= date_limit) then
      begin
        WeedArray[j].Status := wsWeekly;
        if Verbose then
          writeln('    ' + WeedArray[j].Filename + ' assigned to WEEKLY');
        date_search := Trunc(WeedArray[j].Filedate - 1.0);
        while DayOfWeek(date_search) > 1 do // Bump down to Sunday
          date_search := date_search - 1.0;
        if Debug then
          writeln('    Date search is ' + FormatDateTime('yyyy/mm/dd hh:nn',date_search));
      end;

  { Process daily files  }
  if Verbose then
    writeln('Processing daily files');
  date_limit := date_now - DaysToKeep - 0.5;
  date_search := date_now;
  date_search := ComposeDateTime(date_search,
                                 EncodeTime(23,59,59,999));
  if Debug then
    begin
      writeln('    Date limit  is ' + FormatDateTime('yyyy/mm/dd hh:nn',date_limit));
      writeln('    Date search is ' + FormatDateTime('yyyy/mm/dd hh:nn',date_search));
    end;
  for j := 0 to Length(WeedArray) - 1 do
    if (WeedArray[j].Filedate <= date_search) and (WeedArray[j].Filedate >= date_limit) then
      begin
        WeedArray[j].Status := wsDaily;
        date_search := date_search - 1.0;
        if Verbose then
          writeln('    ' + WeedArray[j].Filename + ' assigned to DAILY');
        if Debug then
          writeln('    Date search is ' + FormatDateTime('yyyy/mm/dd hh:nn',date_search));
      end;

  { Fix up files to delete }
  for j := 0 to Length(WeedArray) - 1 do
    if WeedArray[j].Status = wsUnknown then
      begin
        WeedArray[j].Status := wsDelete;
        if Verbose then
          writeln(WeedArray[j].Filename + ' becomes DELETED');
      end;

  { Finally, delete the files we don't need }
  for j := 0 to Length(WeedArray) - 1 do
    if WeedArray[j].Status = wsDelete then
      begin
        delete_flag := false;
        if ConfirmDeletes then
          begin
            write('Delete ' + WeedArray[j].Filename + ' ? ');
            readln(input);
            if (UpperCase(input) = 'Y') or (UpperCase(input) = 'YES') then
              delete_flag := true;
          end
        else
          delete_flag := true;
        if delete_flag then
          begin
            DeleteFile(WeedArray[j].Filename);
            writeln('    Deleted ' + WeedArray[j].Filename);
          end
        else
          write('Cancelled delete of ' + WeedArray[j].Filename);
      end;

  if Verbose then
    Writeln;
end;


{ Process one of the numeric options with range checks }

procedure TWeed.ProcessNumericOption(ShortSwitch: char; LongSwitch: string; var variable: integer; lowlimit: integer; highlimit: integer);
var OptVal:   String;
    OptNum:   integer;
    ErrorMsg: string;
begin
  ErrorMsg := '';
  if HasOption(ShortSwitch,LongSwitch) then
    begin
      OptVal := GetOptionValue(ShortSwitch,LongSwitch);
      try
        OptNum := StrToInt(OptVal);
      except
        ErrorMsg := 'Invalid numeric value ' + Optval + ' on option -' +
                    ShortSwitch + '/--' + LongSwitch;
      end;
      if ErrorMsg = '' then
        if (OptNum >= lowlimit) and
           (OptNum <= highlimit) then
          variable := OptNum
        else
          ErrorMsg := 'Numeric value not in the allowed range ' + IntToStr(lowlimit) +
                      '-' + IntToStr(highlimit) + ' on option -' + ShortSwitch +
                      '/--' + LongSwitch;
      if ErrorMsg<>'' then
        raise Exception.Create(ErrorMsg);
    end;
end;


{ Sort the weed array }

procedure TWeed.SortWeedArray;
var sorted: boolean;
    i:      integer;
    temp:   TWeedFile;
begin
  if Verbose then
    writeln('Sorting file list...');
  repeat
    sorted := true; // Assume sorted for now
    for i := Low(WeedArray) to High(WeedArray)-1 do
      if WeedArray[i].Filedate < WeedArray[i+1].Filedate then
        begin
          sorted := false;
          temp := WeedArray[i];
          WeedArray[i] := WeedArray[i+1];
          WeedArray[i+1] := temp;
        end;
  until sorted;
end;

{ Show some basic help if no parameters have been specified at all }

procedure TWeed.WriteBasicHelp;
begin
  writeln('WEED V1.0 Copyright (C)2017 Templar Mace Limited');
  writeln('');
  writeln('For help, type  weed --help');
end;


{ Long help if the --help flag has been used }

procedure TWeed.WriteHelp;
begin
  { add your help code here }
  writeln('WEED V1.0 Copyright (C)2017 Templar Mace Limited');
  writeln('');
  writeln('Usage: ');
  writeln('');
  writeln('    weed filespec [filespec] [options]');
  writeln('    weed --help');
  writeln('');
  writeln('Filespec is filename encoded with $fields in the following example:');
  writeln('');
  writeln('    weed mysql_dump_sales_$YYYY$MM$DD_$HH$NN.sql.gz');
  writeln('');
  writeln('Will match the following filenames:');
  writeln('');
  writeln('    mysql_dump_sales_20161122_0700.sql.gz');
  writeln('    mysql_dump_sales_20170901_1422.sql.gz');
  writeln('');
  writeln('The following $fields are defined:');
  writeln('');
  writeln('    $YYYY Year with 4 digits');
  writeln('    $YY   Year with 2 digits');
  writeln('    $MM   Month with 2 digits');
  writeln('    $DD   Day of month with 2 digits');
  writeln('    $HH   Hour in 24 hour format with 2 digits');
  writeln('    $NN   Minutes with 2 digits');
  writeln('    $SS   Seconds with 2 digits');
  writeln('');
  writeln('OPTION           FUNCTION                       DEFAULT');
  writeln('------------     -------------------------      -------');
  writeln('-d n,--days=n    Number of days to keep         ' + IntToStr(DEFAULT_DAYS_TO_KEEP));
  writeln('-e,--debug       Print additional debug info    N/A');
  writeln('-f,--filedate    Ignore $fields, use file date  N/A');
  writeln('-h,--help        Show help intructions          N/A');
  writeln('-m n,--months=n  Number of months to keep       ' + IntToStr(DEFAULT_MONTHS_TO_KEEP));
  writeln('-n,--nocomfirm   No confirm of deletes          N/A');
  writeln('-q,--quarters=n  Number of quarters to keep     ' + IntToStr(DEFAULT_QUARTERS_TO_KEEP));
  writeln('-v,--verbose     Print additional info          N/A');
  writeln('-w n,--weeks=n   Number of weeks to keep        ' + IntToStr(DEFAULT_WEEKS_TO_KEEP));
end;

var
  Application: TWeed;

{$R *.res}

begin
  Application:=TWeed.Create(nil);
  Application.Run;
  Application.Free;
end.

