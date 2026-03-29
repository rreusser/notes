'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zpbequ = require( './zpbequ.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zpbequ, 'ndarray', ndarray );


// EXPORTS //

module.exports = zpbequ;
