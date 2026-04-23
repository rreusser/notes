'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dgesc2 = require( './dgesc2.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dgesc2, 'ndarray', ndarray );


// EXPORTS //

module.exports = dgesc2;
