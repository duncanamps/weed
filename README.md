# lacogen1
weed - archive thinning tool - v1.1

#### Synopsis
weed is a command line driven tool to thin out archive files which are typically created on a daily basis. It will allow daily
files to be held for so many days, weekly files to be held for so many weeks, and so on. The intended outcome
is to stop disk consumption growing out of control.

Please be aware that weed deletes files and does not make any provision for their recovery.

#### Development Status
This is working software and has been in daily use since 2017.

#### Development Requirements
To compile this software, you will need Lazarus 2.x or later. It has been tested on Windows and Linux. As it is
only a simple text and file based application, it should be relatively easy to recompile on other hosts which are
supported by the Lazarus ecosystem.

#### Folder Structure
Folders are organised as follows:

* root/ - The Lazarus project files, licence and .gitignore
  * binaries/ - The main executable file, Linux and Windows are included (both 64 bit)
  * units/ - The PASCAL units which make up the core of the software. Most give a description in the header

#### Known Issues 
* None known

#### Author
Duncan Munro
