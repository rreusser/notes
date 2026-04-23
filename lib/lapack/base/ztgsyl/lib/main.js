'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var ztgsyl = require( './ztgsyl.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( ztgsyl, 'ndarray', ndarray );


// EXPORTS //

module.exports = ztgsyl;
