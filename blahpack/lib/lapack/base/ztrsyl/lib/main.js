'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var ztrsyl = require( './ztrsyl.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( ztrsyl, 'ndarray', ndarray );


// EXPORTS //

module.exports = ztrsyl;
