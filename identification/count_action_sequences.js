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
 * parameters), and the value is a dictionary that contains all parameter
 * assignments.
 * The parameter assignment is defined by increasing parameter indices.
 * As an example, for the action sequence [pick-up,stack], a possible value for
 * the parameters is [ [1], [1, 2] ], meaning that the first argument of stack
 * is the same as the (first) argument of pick-up.
 * The emitted values have the following structure:
 * { [[1,1],[2]]: { count: 1, parameters: [[1,1],[2]] },
 *   [[1,2],[3]]: { count: 1, parameters: [[1,2],[3]] } }
 */
var findSequencesInPlan = function() {
  // loop over all starting points for sequences
  for (var start = 0; start < this.actions.length - 1; start++) {
    // loop over all sequences that start at the given point
    for (var length = 2; length <= maxSequenceLength; length++) {
      if (start + length > this.actions.length) {
        break;
      }
      var actions = [];
      var parameters = [];
      // loop over action sequence to collect parameters
      for (var action_i = 0; action_i < length; action_i++) {
        action = this.actions[start + action_i];
        actions.push(action.operator);
        Array.prototype.push.apply(parameters, action.parameters);
      }
      var assignmentsList = [[]];
      var enumerationsList = [[]];
      var nextParams = [0];
      // loop over all parameters of the sequence
      for ( var parameter_i = 0;
            parameter_i < parameters.length;
            parameter_i++) {
        //var newAssignmentsList = assignmentsList.slice();
        //var newEnumerationsList = enumerationsList.slice();
        var newAssignmentsList = [];
        var newEnumerationsList = [];
        var newNextParams = [];
        for ( var assignments_i = 0;
              assignments_i < assignmentsList.length;
              assignments_i++) {
          var assignment = assignmentsList[assignments_i];
          var enumeration = enumerationsList[assignments_i];
          var nextParam = nextParams[assignments_i];
          for ( var possible_p = 0; possible_p < nextParam; possible_p++) {
            if (parameters[parameter_i] == assignment[possible_p]) {
              newAssignmentsList.push(assignment);
              var newEnumeration = enumerationsList[assignments_i].slice();
              newEnumeration.push(possible_p + 1);
              newEnumerationsList.push(newEnumeration);
              newNextParams.push(nextParam);
            }
          }
          var newAssignment = assignment.slice();
          newAssignment.push(parameters[parameter_i]);
          newAssignmentsList.push(newAssignment);
          var newEnumeration = enumeration.slice();
          newEnumeration.push(nextParam + 1);
          newEnumerationsList.push(newEnumeration);
          newNextParams.push(nextParam + 1);
          nextParam++;
        }
        assignmentsList = newAssignmentsList;
        enumerationsList = newEnumerationsList;
        nextParams = newNextParams;
      }
      var result = { actions: actions, totalCount: 1 };
      // reconstruct list of lists of parameters for each list of parameters
      // each action has a list of parameters; so far, the enumerationsList is
      // a single list; the individual actions are not split
      for (var enum_i = 0; enum_i < enumerationsList.length; enum_i++) {
        enumeration = enumerationsList[enum_i];
        newEnumeration = [];
        var nextActionParam = 0;
        for (var action_i = 0; action_i < length; action_i++) {
          action_num_params = this.actions[start + action_i].parameters.length;
          actionEnum = enumeration.slice(
              nextActionParam, nextActionParam + action_num_params);
          newEnumeration.push(actionEnum);
          nextActionParam += action_num_params;
        }
        result[newEnumeration] = {
          'parameters': newEnumeration,
          'count': 1
        };
      }
      emit(actions.join(), result);

    }
  }
}

/* The Reduce part of MapReduce.
 * This function counts all occurences of each parameter assignment.
 * It collects them in a dictionary that contains the count of the parameter
 * assignment and the parameter assignment itself.
 */
var countSequences = function(key, vals) {
  parameterAssignments = { totalCount: 0, actions: vals[0].actions };
  for (var i = 0; i < vals.length; i++) {
    parameterAssignments['totalCount'] += vals[i]['totalCount'];
    Object.keys(vals[i]).forEach(
      function(paramKey, index) {
        if (paramKey == 'actions' || paramKey == 'totalCount') {
          return;
        }
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
    domain: domain,
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


function get_macros(domain_name, max_length) {
  var out = 'action_sequences_' + domain_name;
  var domain = db.domains.findOne(
      { name: domain_name, augmented: {$ne : true}})['_id']
  db.solutions.mapReduce(
      findSequencesInPlan,
      countSequences,
      { out: out,
        query: { "domain": domain, "use_for_macros": true },
        finalize: cleanupResult,
        scope: { domain: domain, maxSequenceLength: max_length }
      }
  )
}
