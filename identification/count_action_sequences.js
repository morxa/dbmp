/*
 *  count_action_sequences.js
 *  Created:  Tue 15 Nov 2016 02:08:51 PM CET
 *  Copyright  2016  Till Hofmann <hofmann@kbsg.rwth-aachen.de>
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

/* The Map part of MapReduce.
 * This function finds all action sequences of length 2 to maxSequenceLength
 * and determines common parameters in each sequence.
 * It emits (key,value) pairs, where the key is the action sequences (without
 * parameters), and the value is the most specific parameterAssignment possible.
 * The parameter assignment is defined by increasing parameter indices.
 * As an example, for the action sequence [pick-up,stack], a possible value for
 * the parameters is [ [1], [1, 2] ], meaning that the first argument of stack
 * is the same as the (first) argument of pick-up.
 */
var findSequencesInPlan = function() {
  for (var i = 0; i < this.actions.length - 1; i++) {
    for (var l = 2; l <= maxSequenceLength; l++) {
      if (i + l > this.actions.length) {
        break;
      }
      // create a new action sequence
      var sequence = [];
      var actions = [];
      var parameterAssignment = [];
      var nextParam = 1;
      for (var j = 0; j < l; j++) {
        sequence.push(this.actions[i+j].operator)
        // compare this action's parameters with the parameters of the previous
        // actions
        actionParams = [];
        actionParamLoop:
        for (var k = 0; k < this.actions[i+j].parameters.length; k++) {
          // loop over actions in this sequence prior to this action
          for (var m = 0; m < j; m++) {
            // loop over that action's parameters
            for (var n = 0; n < this.actions[i+m].parameters.length; n++) {
              if (this.actions[i+m].parameters[n] ==
                  this.actions[i+j].parameters[k]) {
                actionParams.push(parameterAssignment[m][n]);
                continue actionParamLoop;
              }
            }
          }
          actionParams.push(nextParam++);
        }
        parameterAssignment.push(actionParams);
      }
      var val = { actions: sequence, totalCount: 1 };
      val[parameterAssignment] = {
        'parameters': parameterAssignment,
        'count': 1
      };
      emit(sequence.join(), val);
    }
  }
}

/* The Reduce part of MapReduce.
 * This function counts all occurences of each parameter assignment.
 * It collects them in a dictionary that contains the count of the parameter
 * assignment and the parameter assignment itself.
 */
var countSequences = function(key, vals) {
  parameterAssignments = { totalCount: 0 };
  for (var i = 0; i < vals.length; i++) {
    parameterAssignments['totalCount'] += vals[i]['totalCount'];
    Object.keys(vals[i]).forEach(
      function(paramKey, index) {
        if (parameterAssignments.hasOwnProperty(paramKey)) {
          parameterAssignments[paramKey]['count'] += vals[i][paramKey]['count'];
        } else {
          parameterAssignments[paramKey] = vals[i][paramKey];
        }
      }
    )
  }
  return parameterAssignments;
}

/* The Finalize part of MapReduce.
 * This function does some data cleanup on the result. It changes the dictionary
 * with stringified parameter assignment as keys into a list, e.g.:
 * { totalCount: 12, '1,1,2': { count: 4, parameters: [ [1], [1, 2] ] } }
 * => { totalCount: 12, parameters: [{ count: 4, assignment: [ [1], [1, 2] ] }]}
 */
var cleanupResult = function(key, actionSequence) {
  cleanedResult = {
    totalCount: actionSequence['totalCount'],
    actions: actionSequence['actions'],
    parameters: []
  };
  Object.keys(actionSequence).forEach(
      function(seqKey, index) {
        if (seqKey == 'totalCount' || seqKey == 'actions') {
          return;
        }
        cleanedResult['parameters'].push(
            { 'assignment': actionSequence[seqKey]['parameters'],
              'count': actionSequence[seqKey]['count'],
            }
        )
      })
  return cleanedResult;
}


function get_macros(domain, max_length, out) {
  db.solutions.mapReduce(
      findSequencesInPlan,
      countSequences,
      { out: out,
        query: { "domain": domain, "use_for_macros": true },
        finalize: cleanupResult,
        scope: { maxSequenceLength: max_length }
      }
  )
}
