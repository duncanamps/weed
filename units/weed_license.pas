unit weed_license;

{$mode ObjFPC}{$H+}

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

const
  CRLF = #13 + #10;

  COPYRIGHT_MSG = 'WEED V1.1 Copyright (C)2017-2022 Duncan Munro';

  LICENSE_MSG =
    COPYRIGHT_MSG + CRLF + CRLF +
    'Disclaimer of Warranty' + CRLF +
    '----------------------' + CRLF +
    'THERE IS NO WARRANTY FOR THE PROGRAM, TO THE EXTENT PERMITTED BY' + CRLF +
    'APPLICABLE LAW.  EXCEPT WHEN OTHERWISE STATED IN WRITING THE COPYRIGHT' + CRLF +
    'HOLDERS AND/OR OTHER PARTIES PROVIDE THE PROGRAM "AS IS" WITHOUT WARRANTY' + CRLF +
    'OF ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING, BUT NOT LIMITED TO,' + CRLF +
    'THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR' + CRLF +
    'PURPOSE.  THE ENTIRE RISK AS TO THE QUALITY AND PERFORMANCE OF THE PROGRAM' + CRLF +
    'IS WITH YOU.  SHOULD THE PROGRAM PROVE DEFECTIVE, YOU ASSUME THE COST OF' + CRLF +
    'ALL NECESSARY SERVICING, REPAIR OR CORRECTION.' + CRLF + CRLF +
    'Redistribution' + CRLF +
    '--------------' + CRLF +
    'This program is free software: you can redistribute it and/or modify' + CRLF +
    'it under the terms of the GNU General Public License as published by' + CRLF +
    'the Free Software Foundation, either version 3 of the License, or' + CRLF +
    '(at your option) any later version.' + CRLF + CRLF +
    'This program is distributed in the hope that it will be useful,'  + CRLF +
    'but WITHOUT ANY WARRANTY; without even the implied warranty of'  + CRLF +
    'MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the' + CRLF +
    'GNU General Public License for more details.' + CRLF + CRLF +
    'You should have received a copy of the GNU General Public License' + CRLF +
    'along with this program.  If not, see <https://www.gnu.org/licenses/>.' + CRLF;

  HELP_PREAMBLE =
    COPYRIGHT_MSG + CRLF + CRLF +
    'Usage: ' + CRLF + CRLF +
    '    weed filespec [filespec] [options]' + CRLF +
    '    weed --help' + CRLF + CRLF +
    'Filespec is filename encoded with $fields in the following example:' + CRLF + CRLF +
    '    weed mysql_dump_sales_$YYYY$MM$DD_$HH$NN.sql.gz' + CRLF + CRLF +
    'Will match the following filenames:' + CRLF + CRLF +
    '    mysql_dump_sales_20161122_0700.sql.gz' + CRLF +
    '    mysql_dump_sales_20170901_1422.sql.gz' + CRLF + CRLF +
    'You can also use the normal wildcards such as ? and *' + CRLF + CRLF +
    'The following $fields are defined:' + CRLF + CRLF +
    '    $YYYY Year with 4 digits' + CRLF +
    '    $YY   Year with 2 digits' + CRLF +
    '    $MM   Month with 2 digits' + CRLF +
    '    $DD   Day of month with 2 digits' + CRLF +
    '    $HH   Hour in 24 hour format with 2 digits' + CRLF +
    '    $NN   Minutes with 2 digits' + CRLF +
    '    $SS   Seconds with 2 digits' + CRLF;

implementation
  { Empty }

end.

