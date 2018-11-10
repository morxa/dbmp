#! /usr/bin/env python3
# -*- coding: utf-8 -*-
# vim:fenc=utf-8
#
#  Created:  Tue 16 Jan 2018 15:16:12 CET
#  Copyright  2018  Till Hofmann <hofmann@kbsg.rwth-aachen.de>
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

"""
Upload the given MacroFF macro file to the database.
"""

import argparse
import pymongo

def main():
    arg_parser = argparse.ArgumentParser(
        description="Upload the given MacroFF macro file to the database")
    arg_parser.add_argument('--host', type=str, default='enterprise',
                            help='Host to connect to')
    arg_parser.add_argument('-u', '--user', type=str, default='planner',
                            help='database user')
    arg_parser.add_argument('-p', '--password', type=str,
                            help='password of the database user')
    arg_parser.add_argument('domain', type=str,
                            help='the domain name')
    arg_parser.add_argument('macro_file', type=argparse.FileType('r'))
    args = arg_parser.parse_args()
    client = pymongo.MongoClient(host=args.host)
    db = client.macro_planning
    db.authenticate(args.user, args.password)
    domain_entry = db.domains.find_one({'name': args.domain,
                                        'augmented': { '$ne': True }})
    assert domain_entry, \
            'Could not find domain with name {} in db'.format(args.domain)
    domain_id = domain_entry['_id']
    print('Uploading macro file "{}" for domain "{}"'.format(
        args.macro_file.name, domain_id))
    db.macros.insert_one({'name': 'macroff-' + str(domain_id),
                          'domain': domain_id,
                          'planner': 'macroff',
                          'raw': args.macro_file.read() })

if __name__ == '__main__':
    main()
