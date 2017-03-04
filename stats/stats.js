/*
 *  stats.js
 *  Created:  Mon 21 Nov 2016 16:35:20 CET
 *  Copyright  2016,2017  Till Hofmann <hofmann@kbsg.rwth-aachen.de>
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Library General Public License for more details.
 *
 *  Read the full text in the LICENSE.GPL file in the doc directory.
 */


function get_stats() {
  return db.solutions.aggregate(
      [
      { $project: {
          planner: 1,
          problem: 1,
          resources: { $ifNull: [ "resources", [3600]]}
                  }
      },
      { $out: "aggregation_test" }
      ],
      { explain: true }
  );
}

function get_avgs() {
  db.solutions.aggregate(
      [
        { $match: { planner: { $exists: 1}, domain: {
    $exists: 1}, problem: { $exists: 1}}},
        { $project: {
            planner: 1,
            problem: 1,
            domain: 1,
            resources: { $ifNull: [ "$resources", [1800]]}}
        },
        { $unwind: "$resources" },
        { $group: {
            "_id": {
              planner: "$planner", domain: "$domain", problem: "$problem"},
            "cpu_time": { $first: "$resources" }}
        },
        { $group: {
            "_id": {
              planner: "$_id.planner", domain: "$_id.domain"},
            avg_time: { $avg: "$cpu_time"}}
        },
        { $project: {
            "planner": "$_id.planner",
            "domain": "$_id.domain",
            "_id": 0,
            avg_time: "$avg_time"
          }
        },
        { $out: "planning_time_avgs" }
      ])
}

function get_avgs_solved() {
  db.solutions.aggregate(
      [
        { $match: { planner: { $exists: 1}, domain: {
    $exists: 1}, problem: { $exists: 1}, error: { $exists: 0}}},
        { $project: {
            planner: 1,
            problem: 1,
            domain: 1,
            resources: 1}
        },
        { $unwind: "$resources" },
        { $group: {
            "_id": {
              planner: "$planner", domain: "$domain", problem: "$problem"},
            "cpu_time": { $first: "$resources" }}
        },
        { $group: {
            "_id": {
              planner: "$_id.planner", domain: "$_id.domain"},
            avg_time: { $avg: "$cpu_time"}}
        },
        { $project: {
            "planner": "$_id.planner",
            "domain": "$_id.domain",
            "_id": 0,
            avg_time: "$avg_time"
          }
        },
        { $out: "planning_time_avgs_solved" }
      ])
}
function get_avg_plan_length() {
  db.solutions.aggregate(
      [
        { $match: { planner: { $exists: 1}, domain: {
    $exists: 1}, problem: { $exists: 1}, error: { $exists: 0}, actions: { $exists: 1}}},
        { $project: {
            planner: 1,
            problem: 1,
            domain: 1,
            num_actions: { $size: "$actions"}
          }
        },
        { $group: {
            "_id": {
              planner: "$planner", domain: "$domain"},
            avg_plan_length: { $avg: "$num_actions"}}
        },
        { $project: {
            "planner": "$_id.planner",
            "domain": "$_id.domain",
            "_id": 0,
            avg_plan_length: "$avg_plan_length"
          }
        },
        { $out: "planning_plan_lengths" }
      ])
}


function get_num_completed() {
  db.solutions.aggregate(
      [
        { $match: { error: { $exists: 0 }}},
        { $group: {
            _id: { planner: "$planner", domain: "$domain" },
            count: { $sum: 1}
          }
        },
        { $project: {
            planner: "$_id.planner",
            domain: "$_id.domain",
            count: "$count",
            "_id": 0,
            }
        },
        { $out: "planning_completions" }
      ]
  )
}

function sort_solutions() {
  db.solutions.aggregate(
      [
        { $match: { planner: { $exists: 1}, domain: {
    $exists: 1}, problem: { $exists: 1}}},
        { $project: {
            planner: 1,
            problem: 1,
            domain: 1,
            resources: { $ifNull: [ "$resources", [1800]]}}
        },
        { $unwind: "$resources" },
        { $group: {
            "_id": {
              planner: "$planner", domain: "$domain", problem: "$problem"},
            "cpu_time": { $first: "$resources" }}
        },
        { $sort: { cpu_time: 1 } },
        { $out: "sorted_solutions" }
      ]
  );
}

function aggregate_runtimes() {
  db.solutions.aggregate(
      [
        { $match: { use_for_macros: { $ne: true }}},
        { $project: {
            runtime: {
              $subtract: [ "$end_time", "$start_time" ]
              }
            }
        },
        { $group: { "_id": null,
                    total_time: { $sum: "$runtime" }
                  }
        },
        { $out: "runtime_sum" }
      ]
  );
}

function get_best_domain(planner, domain_name) {
  get_avgs();
  cursor = db.planning_time_avgs.find().sort({'avg_time': 1});
  while (cursor.hasNext()) {
    var stats = cursor.next();
    if (stats['planner'] != 'ff') {
      continue;
    }
    if (db.domains.findOne(
          {'name': domain_name, '_id': stats['domain'], 'augmented': true}))
    {
      printjson(stats);
      break;
    }
  }
}

function get_shortest_plans_domain(planner, domain_name) {
  get_avg_plan_length();
  cursor = db.planning_plan_lengths.find().sort({'avg_plan_length': 1});
  while (cursor.hasNext()) {
    var stats = cursor.next();
    if (stats['planner'] != 'ff') {
      continue;
    }
    if (db.domains.findOne(
          {'name': domain_name, '_id': stats['domain'], 'augmented': true}))
    {
      printjson(stats);
      break;
    }
  }
}
