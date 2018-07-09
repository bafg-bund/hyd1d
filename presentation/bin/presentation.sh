#!/bin/bash
cd /srv/cifs-mounts/WeberA_home/WeberA/hyd1d

# load modules
module add i4/applications/R-3.5.0
module add i4/applications/pandoc-2.1.1

# render the presentation as html
Rscript -e "rmarkdown::render('presentation/presentation.Rmd', output_file = 'index.html', output_dir = '/home/WeberA/public_html/hyd1d/presentation/')"

# and as pdf
#Rscript -e "rmarkdown::render('presentation/presentation.Rmd', output_format = 'pdf_document', output_file = 'presentation.pdf', output_dir = '/home/WeberA/public_html/hyd1d/presentation/')"

# copy external image files
cp -rf presentation/*.png /home/WeberA/public_html/hyd1d/presentation/
cp -rf presentation/*.css /home/WeberA/public_html/hyd1d/presentation/

# set SE linux permissions
chcon -R -t httpd_user_content_t /home/WeberA/public_html/

exit 0