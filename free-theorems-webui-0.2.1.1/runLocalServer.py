# This script starts a CGI server.
#
# It expects 2-3 arguments:
#    1. path to the folder containing the "free-theorems-webui.cgi" binary.
#    2. path to the folder containing static files (like "style.css" etc.)
#    3. optional port number (default is 8080)
#
from CGIHTTPServer import *
from BaseHTTPServer import *

import sys, os
import webbrowser

bin_dir = sys.argv[1]
data_dir = sys.argv[2]

if len(sys.argv) > 3:
   port = int(sys.argv[3])
else:
   port = 8080


def is_cgi(path):
   return path == "/" or path.startswith("/?")

class CustomHandler (CGIHTTPRequestHandler):

   def is_cgi(self):
      if is_cgi(self.path):
         self.cgi_info = ("", self.path) # no directory portion, no filename (has to be set)
         return True

   def translate_path(self, path):
      if is_cgi(path):
         return os.path.join (bin_dir, "free-theorems-webui.cgi")
      else:
         return os.path.join (data_dir, *path.split("/"))


server = HTTPServer(('',port), CustomHandler)

url = "http://localhost:%d" % server.server_port
print "Starting server at %s..." % url

webbrowser.open(url)
server.serve_forever()
