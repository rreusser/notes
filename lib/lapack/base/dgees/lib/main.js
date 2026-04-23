'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dgees = require( './dgees.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dgees, 'ndarray', ndarray );


// EXPORTS //

module.exports = dgees;
