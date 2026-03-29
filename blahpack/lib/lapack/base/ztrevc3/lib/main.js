'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var ztrevc3 = require( './ztrevc3.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( ztrevc3, 'ndarray', ndarray );


// EXPORTS //

module.exports = ztrevc3;
