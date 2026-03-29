'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zrotg = require( './zrotg.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zrotg, 'ndarray', ndarray );


// EXPORTS //

module.exports = zrotg;
