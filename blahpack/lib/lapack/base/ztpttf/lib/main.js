'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var ztpttf = require( './ztpttf.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( ztpttf, 'ndarray', ndarray );


// EXPORTS //

module.exports = ztpttf;
