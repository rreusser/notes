'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var zgetrf2 = require( './zgetrf2.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zgetrf2, 'ndarray', ndarray );


// EXPORTS //

module.exports = zgetrf2;
