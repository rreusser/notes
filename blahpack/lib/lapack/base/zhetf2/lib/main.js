'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zhetf2 = require( './zhetf2.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zhetf2, 'ndarray', ndarray );


// EXPORTS //

module.exports = zhetf2;
