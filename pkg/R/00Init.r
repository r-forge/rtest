######################################################################
##  rtest : unit and system testing for R
##  Copyright (C) 2010  Mario Frasca
##  Copyright (C) 2003-2009  Thomas Koenig, Matthias Burger, Klaus Juenemann
##
##  This program is free software; you can redistribute it and/or modify
##  it under the terms of the GNU General Public License as published by
##  the Free Software Foundation; version 2 of the License.
##
##  This program is distributed in the hope that it will be useful,
##  but WITHOUT ANY WARRANTY; without even the implied warranty of
##  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##  GNU General Public License for more details.
##
##  You should have received a copy of the GNU General Public License
##  along with this program; if not, write to the Free Software
##  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

##  $Id$


.onLoad <- function(libname, pkgname)
{
  ##@bdescr
  ## Internal Function.
  ## Not to be called by users.
  ##@edescr

  ##  load required packages
  errMsg <- paste("\nLoading required package 'methods' failed. rtest could not be loaded.",
                  "\nCheck your library installation path.\n")
  require(methods) || stop(errMsg)

  
  runitVersion <- packageDescription("rtest", lib.loc=libname, fields="Version")
  ##  avoid cmd check NOTEs
  assign(".testLogger", NULL, envir=.GlobalEnv)
  ##  add options to R's global options list
  .buildRUnitOptions()
}

.onUnload <- function(libpath) {
  ##  drop RUnit specific options from global options list
  options("RUnit"=NULL)
}

