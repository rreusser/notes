
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zlaqhp = require( './zlaqhp.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zlaqhp, 'ndarray', ndarray );


// EXPORTS //

module.exports = zlaqhp;
