
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zgetf2 = require( './zgetf2.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zgetf2, 'ndarray', ndarray );


// EXPORTS //

module.exports = zgetf2;
