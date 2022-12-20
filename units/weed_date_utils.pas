unit weed_date_utils;

{$mode objfpc}{$H+}

{
    weed - Thins out regular archive files
    Copyright (C)2017-2022 Duncan Munro

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.

    Contact: Duncan Munro  duncan@duncanamps.com
}

interface

uses
  Classes, SysUtils;

function SubtractDays(src: TDateTime; days: integer): TDateTime;
function SubtractWeeks(src: TDateTime; weeks: integer): TDateTime;
function SubtractMonths(src: TDateTime; months: integer): TDateTime;
function SubtractQuarters(src: TDateTime; quarters: integer): TDateTime;
function YearStartPreceding(src: TDateTime): TDateTime;
function QuarterStartPreceding(src: TDateTime): TDateTime;
function MonthStartPreceding(src: TDateTime): TDateTime;


implementation

uses
  DateUtils;

{ Subtract a number of days from a DateTime }

function SubtractDays(src: TDateTime; days: integer): TDateTime;
begin
  result := src - days;
end;


{ Subtract a number of weeks from a DateTime }

function SubtractWeeks(src: TDateTime; weeks: integer): TDateTime;
begin
  result := SubtractDays(src,weeks*7);
end;


{ Subtract a number of months from a DateTime }

function SubtractMonths(src: TDateTime; months: integer): TDateTime;
var d,m,y,hh,mm,ss,ms: word;
begin
  DecodeDate(src,y,m,d);
  DecodeTime(src,hh,mm,ss,ms);
  while months > 0 do
    begin
      Dec(m);
      if (m <= 0) then
        begin
          m := m + 12;
          Dec(y);
        end;
      Dec(months);
    end;
  // Take account of day of month might not be valid any more!
  // e.g. Sub 3 months from 31st May gives 31st Feb == Invalid
  while not IsValidDate(y,m,d) do
    if d > 28 then
      Dec(d)
    else
      raise Exception.Create('Internal error - Invalid date calculation');
  result := ComposeDateTime(EncodeDate(y,m,d),EncodeTime(hh,mm,ss,ms));
end;


{ Subtract a number of quarters from a DateTime }

function SubtractQuarters(src: TDateTime; quarters: integer): TDateTime;
begin
  result := SubtractMonths(src,quarters*3);
end;


{ Decide if DateTime represents the start of a year }

function IsStartOfYear(src: TDateTime): boolean;
var y,m,d,h,n,s,ms: word;
begin
  DecodeDate(src,y,m,d);
  DecodeTime(src,h,n,s,ms);
  result := (m = 1) and
            (d = 1) and
            (h = 0) and
            (n = 0) and
            (s = 0) and
            (ms = 0);
end;


{ Decide if DateTime represents the start of a quarter }

function IsStartOfQuarter(src: TDateTime): boolean;
var y,m,d,h,n,s,ms: word;
begin
  DecodeDate(src,y,m,d);
  DecodeTime(src,h,n,s,ms);
  result := (m in [1,4,7,10]) and
            (d = 1) and
            (h = 0) and
            (n = 0) and
            (s = 0) and
            (ms = 0);
end;


{ Decide if DateTime represents the start of a month }

function IsStartOfMonth(src: TDateTime): boolean;
var y,m,d,h,n,s,ms: word;
begin
  DecodeDate(src,y,m,d);
  DecodeTime(src,h,n,s,ms);
  result := (d = 1) and
            (h = 0) and
            (n = 0) and
            (s = 0) and
            (ms = 0);
end;


{ Reduce a month number by a given amount }

procedure ReduceMonthByOne(var year: word; var month: word);
begin
  if month = 1 then
    begin
      month := 12;
      Dec(year);
    end
  else
    Dec(month);
end;

procedure ReduceMonth(var year: word; var month: word; reduce: word);
begin
  while reduce > 0 do
    begin
      ReduceMonthByOne(year,month);
      Dec(reduce);
    end;
end;

{ Find the year start preceding the provided date }

function YearStartPreceding(src: TDateTime): TDateTime;
var y,m,d,h,n,s,ms: word;
begin
  DecodeDate(src,y,m,d);
  DecodeTime(src,h,n,s,ms);
  if IsStartOfYear(src) then
    Dec(y)
  else
    begin
      m  := 1;
      d  := 1;
      h  := 0;
      n  := 0;
      s  := 0;
      ms := 0;
    end;
  result := ComposeDateTime(EncodeDate(y,m,d),EncodeTime(h,n,s,ms));
end;


{ Find the quarter start preceding the provided date }

function QuarterStartPreceding(src: TDateTime): TDateTime;
var y,m,d,h,n,s,ms: word;
begin
  DecodeDate(src,y,m,d);
  DecodeTime(src,h,n,s,ms);
  if IsStartOfQuarter(src) then
    ReduceMonth(y,m,3)
  else
    begin
      while not (m in [1,4,7,10]) do
        Dec(m);
      d  := 1;
      h  := 0;
      n  := 0;
      s  := 0;
      ms := 0;
    end;
  result := ComposeDateTime(EncodeDate(y,m,d),EncodeTime(h,n,s,ms));
end;


{ Find the month start preceding the provided date }

function MonthStartPreceding(src: TDateTime): TDateTime;
var y,m,d,h,n,s,ms: word;
begin
  DecodeDate(src,y,m,d);
  DecodeTime(src,h,n,s,ms);
  if IsStartOfMonth(src) then
    ReduceMonthByOne(y,m)
  else
    begin
      d  := 1;
      h  := 0;
      n  := 0;
      s  := 0;
      ms := 0;
    end;
  result := ComposeDateTime(EncodeDate(y,m,d),EncodeTime(h,n,s,ms));
end;


end.

