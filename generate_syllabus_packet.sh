#!/usr/bin/env bash
COURSE=STA9890
CHROME="/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"
BASE_URL='https://michael-weylandt.com/'

as_pdf () {
  "$CHROME" --headless --disable-gpu --print-to-pdf=$1.pdf $BASE_URL/$COURSE/$1.html
}


declare -a PAGES=("syllabus" "resources" "objectives" "reports")
PDFS=("${PAGES[@]}")

mkdir _tmp_syllabuspacket
cd _tmp_syllabuspacket

for page in "${PAGES[@]}"
do
  echo "$page"
  as_pdf "$page"
done

for i in "${!PDFS[@]}"
do
  PDFS[$i]="${PDFS[$i]}.pdf"
done

pdftk "${PDFS[@]}" output "$COURSE"_syllabus_packet.pdf

mv "$COURSE"_syllabus_packet.pdf ..
cd ..
rm -rf _tmp_syllabuspacket
