

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var zhetd2 = require( './zhetd2.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zhetd2, 'ndarray', ndarray );


// EXPORTS //

module.exports = zhetd2;
