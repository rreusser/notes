
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dtfsm = require( './dtfsm.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dtfsm, 'ndarray', ndarray );


// EXPORTS //

module.exports = dtfsm;
