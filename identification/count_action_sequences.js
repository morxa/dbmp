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

/* map function */
var findSequencesInPlan = function() {
  var maxSequenceLength = 4
  var countedSequences = {}
  for (var i = 0; i < this.actions.length - 1; i++) {
    for (var l = 2; l <= maxSequenceLength; l++) {
      if (i + l > this.actions.length) {
        break;
      }
      var sequence = [];
      for (var j = 0; j < l; j++) {
        sequence.push(this.actions[i + j].operator)
      }
      if (countedSequences.hasOwnProperty(sequence)) {
        countedSequences[sequence] += 1;
      } else {
        countedSequences[sequence] = 1;
      }
    }
  }
  for (var key in countedSequences) {
    emit(key, countedSequences[key])
  }
}

var countSequences = function(key, vals) {
  totalCount = 0;
  for (i = 0; i < vals.length; i++) {
    totalCount += vals[i];
  }
  return totalCount;
}

