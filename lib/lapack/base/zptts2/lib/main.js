'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zptts2 = require( './zptts2.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zptts2, 'ndarray', ndarray );


// EXPORTS //

module.exports = zptts2;
