
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zlatps = require( './zlatps.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zlatps, 'ndarray', ndarray );


// EXPORTS //

module.exports = zlatps;
