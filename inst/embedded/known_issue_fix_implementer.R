

# this file attempts to fix several issues on the fly
# this is like community driven hot fixes

############# Fix for ###############
#######
#######  RSelenium - and related packages
#######
#####################################

# Issue 1
# wdman and LICENSE.chromedriver issue
# this can be handled directly in rst_wdman_selenium_launcher
# <><> Fixed


# Issue 2
# wdman expects only "selenium-server-standalone" selenium-server will not work
#TODO many works may be required here
# main issue RSelenium is not working as of now with selenium 4.8
# need to add flag standalone in command
