

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var zgetrf = require( './zgetrf.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zgetrf, 'ndarray', ndarray );


// EXPORTS //

module.exports = zgetrf;
