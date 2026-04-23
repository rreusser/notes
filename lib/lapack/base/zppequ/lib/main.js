'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zppequ = require( './zppequ.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zppequ, 'ndarray', ndarray );


// EXPORTS //

module.exports = zppequ;
