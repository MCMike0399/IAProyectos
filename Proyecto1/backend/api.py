from flask import Flask
import subprocess

app = Flask(__name__)

@app.route('/app')

subprocess.call(['sh','./run.sh']) #Corre el clisp