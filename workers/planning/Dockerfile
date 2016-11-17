#  Created:  Mon 07 Nov 2016 15:44:01 CET
#  Copyright  2016  Till Hofmann <hofmann@kbsg.rwth-aachen.de>
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU Library General Public License for more details.
#
#  Read the full text in the LICENSE.GPL file in the doc directory.

FROM fedora:24
MAINTAINER Till Hofmann <hofmann@kbsg.rwth-aachen.de>

RUN dnf -y --refresh install 'dnf-command(copr)' && \
    dnf -y copr enable thofmann/planner && \
    dnf -y --refresh install \
      fast-downward \
      fast-forward \
      python3-pymongo \
    && \
    dnf clean all
COPY worker.py /usr/bin/
