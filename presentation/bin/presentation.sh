#!/bin/bash
cd /home/WeberA/hyd1d_tmp

# load modules
module add i4/R/OS
module add i4/pandoc/2.3.2

# render the presentation as html
Rscript -e "rmarkdown::render('presentation/presentation.Rmd', output_file = 'presentation_DE.html', output_dir = '/home/WeberA/public_html/hyd1d/articles/')"
Rscript -e "rmarkdown::render('presentation/presentation.Rmd', output_file = 'presentation_DE.html', output_dir = '/home/WeberA/public_html/hydflood/articles/')"

# and as pdf
Rscript -e "rmarkdown::render('presentation/presentation.Rmd', output_format = 'pdf_document', output_file = 'presentation_hyd1d_DE.pdf', output_dir = '/home/WeberA/public_html/hyd1d/downloads/')"

# copy external image files
cp -rf presentation/*.png /home/WeberA/public_html/hyd1d/articles/
cp -rf presentation/*.css /home/WeberA/public_html/hyd1d/articles/
cp -rf presentation/*.mp4 /home/WeberA/public_html/hyd1d/articles/
cp -rf presentation/*.png /home/WeberA/public_html/hydflood/articles/
cp -rf presentation/*.css /home/WeberA/public_html/hydflood/articles/
cp -rf presentation/*.mp4 /home/WeberA/public_html/hydflood/articles/

# set SE linux permissions
chcon -R -t httpd_user_content_t /home/WeberA/public_html/

exit 0