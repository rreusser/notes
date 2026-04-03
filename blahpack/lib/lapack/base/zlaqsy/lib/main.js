
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zlaqsy = require( './zlaqsy.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zlaqsy, 'ndarray', ndarray );


// EXPORTS //

module.exports = zlaqsy;
