

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var zgetrs = require( './zgetrs.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zgetrs, 'ndarray', ndarray );


// EXPORTS //

module.exports = zgetrs;
