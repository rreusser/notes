'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zgees = require( './zgees.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zgees, 'ndarray', ndarray );


// EXPORTS //

module.exports = zgees;
