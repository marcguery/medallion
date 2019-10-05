#!/usr/bin/env python3
import json

if __name__=="__main__":
	medalsfile="rawdata-medals.json"

	medalsdata=json.load(open(medalsfile, 'r'))
	medalscsv="\n".join(str(medal).strip(r"\[|\]") for medal in medalsdata['data'])

	with open("medals.csv", 'w') as f:
		f.write(medalscsv)